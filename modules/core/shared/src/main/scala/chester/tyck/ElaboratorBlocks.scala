package chester.tyck

import scala.collection.mutable
import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, CST, EffectRef, EnumCase, Implicitness, JSImportKind, Param, StmtAST, Telescope}
import chester.error.Span
import chester.tyck.ElabConstraint
import chester.tyck.ElabContext
import chester.uniqid.{Uniqid, UniqidOf}
import chester.utils.HoldNotReadable
import chester.utils.elab.*
import cats.data.NonEmptyVector

/** Statement/block elaboration rules extracted from `Elaborator`. Focuses on block and top-level forms. */
object ElaboratorBlocks:

  trait Helpers:
    def combinedSpan(elems: Seq[CST]): Option[Span]
    def parseEnumTypeParams[M <: SolverModule](elems: Vector[CST], ctx: ElabContext)(using
        module: M,
        solver: module.Solver[ElabConstraint]
    ): Vector[Param]
    def parseRecordFields[M <: SolverModule](elems: Vector[CST], ctx: ElabContext)(using
        module: M,
        solver: module.Solver[ElabConstraint]
    ): Vector[Param]
    def parseEnumCases[M <: SolverModule](elems: Vector[CST], ctx: ElabContext, typeParams: Vector[Param])(using
        module: M,
        solver: module.Solver[ElabConstraint]
    ): Vector[EnumCase]
    def handleDefStatement[M <: SolverModule](
        c: ElabConstraint.Infer,
        elems: Vector[CST],
        span: Option[Span],
        defId: UniqidOf[AST],
        defTypeCell: CellRW[AST],
        ctx: ElabContext,
        defInfoMap: mutable.Map[CST, DefInfo]
    )(using module: M, solver: module.Solver[ElabConstraint]): Result
    def processEffectBody[M <: SolverModule](body: CST.Block, effectRef: EffectRef, ctx: ElabContext)(using
        module: M,
        solver: module.Solver[ElabConstraint]
    ): ElabContext
    def handleLetStatement[M <: SolverModule](
        elems: Vector[CST],
        span: Option[Span],
        ctx: ElabContext,
        resultCell: CellRW[AST],
        inferredTyCell: CellRW[AST]
    )(using module: M, solver: module.Solver[ElabConstraint]): ElabContext
    def normalizeTypeLikeKind(ty: AST): AST
    def substituteSolutions(ast: AST): AST

  /** Handler for top-level inference (file mode). */
  def handleInferTopLevel[M <: SolverModule](c: ElabConstraint.InferTopLevel)(using
      module: M,
      solver: module.Solver[ElabConstraint],
      helpers: Helpers
  ): Result = {
    import module.given

    if module.hasStableValue(solver, c.result) && module.hasStableValue(solver, c.inferredTy) then return Result.Done

    c.cst match
      case CST.Block(elements, tail, span) =>
        elaborateBlockLike(c.ctx, c.result, c.inferredTy, elements, tail, span)

      case CST.SeqOf(elements, span) =>
        val elems = elements.toVector
        elems.headOption match
          case Some(CST.Symbol("def", _)) | Some(CST.Symbol("effect", _)) | Some(CST.Symbol("record", _)) | Some(CST.Symbol("enum", _)) |
              Some(CST.Symbol("coenum", _)) | Some(CST.Symbol("import", _)) =>
            val blockElem = CST.SeqOf(NonEmptyVector.fromVectorUnsafe(elems), span)
            elaborateBlockLike(c.ctx, c.result, c.inferredTy, Vector(blockElem), None, span)
          case Some(CST.Symbol("let", _)) =>
            c.ctx.reporter.report(
              ElabProblem.UnboundVariable("let statement only allowed in block elements, not in expression position", span)
            )
            val metaId = Uniqid.make[AST]
            val ast = AST.Ref(metaId, "let", span)
            module.fill(solver, c.result, ast)
            val metaTy = module.newOnceCell[ElabConstraint, AST](solver)
            module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(metaTy), span))
            Result.Done
          case _ =>
            if elems.headOption.exists { case CST.Symbol("package", _) => true; case _ => false } then
              if elems.length < 2 then
                c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected package name", span))
                module.fill(solver, c.result, AST.Ref(Uniqid.make, "<error>", span))
                module.fill(solver, c.inferredTy, AST.Type(AST.LevelLit(0, None), None))
                Result.Done
              else {
                val pkgName = elems(1) match
                  case CST.Symbol(n, _) => n
                  case _ =>
                    c.ctx.reporter.report(ElabProblem.UnboundVariable("Expected package name", span))
                    "<error>"
                val bodyCst = {
                  if elems.length > 2 then
                    val rest = elems.drop(2)
                    if rest.length == 1 then rest.head
                    else CST.SeqOf(NonEmptyVector.fromVectorUnsafe(rest), helpers.combinedSpan(rest))
                  else CST.Block(Vector.empty, None, span)
                }

                val bodyResult = module.newOnceCell[ElabConstraint, AST](solver)
                val bodyTy = module.newOnceCell[ElabConstraint, AST](solver)
                module.addConstraint(solver, ElabConstraint.InferTopLevel(bodyCst, bodyResult, bodyTy, c.ctx))

                val pkgStmt = StmtAST.Pkg(pkgName, AST.MetaCell(HoldNotReadable(bodyResult), span), span)
                val block = AST.Block(Vector(pkgStmt), AST.MetaCell(HoldNotReadable(bodyResult), span), span)
                module.fill(solver, c.result, block)
                module.fill(solver, c.inferredTy, AST.MetaCell(HoldNotReadable(bodyTy), span))
                Result.Done
              }
            else {
              module.addConstraint(solver, ElabConstraint.Infer(c.cst, c.result, c.inferredTy, c.ctx))
              Result.Done
            }
      case other =>
        module.addConstraint(solver, ElabConstraint.Infer(other, c.result, c.inferredTy, c.ctx))
        Result.Done
  }

  /** Elaborate a block-like group of statements (shared by real blocks and top-level blockish SeqOf forms). */
  def elaborateBlockLike[M <: SolverModule](
      ctx: ElabContext,
      resultCell: CellRW[AST],
      inferredTyCell: CellRW[AST],
      elements: Vector[CST],
      tail: Option[CST],
      span: Option[Span]
  )(using module: M, solver: module.Solver[ElabConstraint], helpers: Helpers): Result = {
    import module.given

    elements.headOption.flatMap(extractPackageName) match
      case Some(pkgName) =>
        val innerResult = module.newOnceCell[ElabConstraint, AST](solver)
        val innerTy = module.newOnceCell[ElabConstraint, AST](solver)
        elaborateBlockLike(ctx, innerResult, innerTy, elements.tail, tail, span)
        val pkgStmt = StmtAST.Pkg(pkgName, AST.MetaCell(HoldNotReadable(innerResult), span), span)
        module.fill(solver, resultCell, AST.Block(Vector(pkgStmt), AST.Tuple(Vector.empty, span), span))
        module.fill(solver, inferredTyCell, AST.TupleType(Vector.empty, span))
        return Result.Done
      case None => ()

    // Check if tail contains a def/let/record statement (which is not allowed)
    tail.foreach { t =>
      if isDefStatement(t) then ctx.reporter.report(ElabProblem.UnboundVariable("def statement not allowed in tail position", t.span))
      if isLetStatement(t) then ctx.reporter.report(ElabProblem.UnboundVariable("let statement not allowed in tail position", t.span))
      if isRecordStatement(t) then ctx.reporter.report(ElabProblem.UnboundVariable("record statement not allowed in tail position", t.span))
      if isImportStatement(t) then ctx.reporter.report(ElabProblem.UnboundVariable("import statement not allowed in tail position", t.span))
      if isEnumStatement(t) then
        ctx.reporter.report(ElabProblem.UnboundVariable("enum/coenum statement not allowed in tail position; terminate with ';'", t.span))
    }

    // TWO-PASS ELABORATION for forward references:
    // Pass 1: Scan elements, collect all def declarations, create placeholder cells (reusing any pre-bound ones)
    var enrichedCtx = ctx
    val defInfoMap = scala.collection.mutable.Map.empty[CST, DefInfo]
    val recordInfoMap = scala.collection.mutable.Map.empty[CST, (String, UniqidOf[AST], module.CellRW[AST])]
    val enumInfoMap = scala.collection.mutable.Map.empty[CST, (String, UniqidOf[AST], Vector[Param], Boolean)]

    elements.foreach {
      case elem @ CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
        // Extract def name
        val elemsVec = seqElems.toVector
        elemsVec match
          case _ +: CST.Symbol(name, _) +: _ =>
            // Reuse existing binding if present (for cross-file/module pre-pass), otherwise create new
            val (defId, defTypeCell) = enrichedCtx.lookup(name) match
              case Some(existingId) =>
                val cell = enrichedCtx.lookupType(existingId).getOrElse(module.newOnceCell[ElabConstraint, AST](solver))
                (existingId, cell)
              case None =>
                val newId = Uniqid.make[AST]
                val cell = module.newOnceCell[ElabConstraint, AST](solver)
                (newId, cell)
            val defResultCell = module.newOnceCell[ElabConstraint, AST](solver)
            // Add to context so it can be referenced (including by other defs)
            enrichedCtx = enrichedCtx.bind(name, defId, defTypeCell).registerDefBody(defId, defResultCell)
            defInfoMap(elem) = DefInfo(name, defId, defTypeCell, defResultCell, span = elem.span)
          case _ => ()
      case CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists { case CST.Symbol("effect", _) => true; case _ => false } =>
        val seqVec = seqElems.toVector
        seqVec.lift(1) match
          case Some(CST.Symbol(name, _)) =>
            enrichedCtx = enrichedCtx.declareEffect(name)
          case _ => ()
      case elem @ CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists { case CST.Symbol("record", _) => true; case _ => false } =>
        val seqVec = seqElems.toVector
        seqVec.lift(1) match
          case Some(CST.Symbol(name, _)) =>
            val recordId = Uniqid.make[AST]
            val ctorTyCell = enrichedCtx.lookup(name).flatMap(enrichedCtx.lookupType).getOrElse(module.newOnceCell[ElabConstraint, AST](solver))
            enrichedCtx = enrichedCtx.registerRecordPlaceholder(name, recordId, ctorTyCell)
            recordInfoMap(elem) = (name, recordId, ctorTyCell)
          case _ => ()
      case elem @ CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists {
            case CST.Symbol("enum", _) | CST.Symbol("coenum", _) => true
            case _                                               => false
          } =>
        val seqVec = seqElems.toVector
        seqVec.lift(1) match
          case Some(CST.Symbol(name, _)) =>
            val typeParams = helpers.parseEnumTypeParams(seqVec, enrichedCtx)
            val enumId = Uniqid.make[AST]
            val isCo = seqVec.headOption.exists { case CST.Symbol("coenum", _) => true; case _ => false }
            enrichedCtx = enrichedCtx.registerEnumPlaceholder(name, enumId, typeParams, isCo)
            enumInfoMap(elem) = (name, enumId, typeParams, isCo)
          case _ => ()
      case _ => ()
    }

    // Pass 2: Elaborate all elements with enriched context and sequential lets
    val elaboratedElemsBuffer = scala.collection.mutable.ArrayBuffer.empty[StmtAST]
    var currentCtx = enrichedCtx

    elements.foreach { elem =>
      val elemResult = {
        defInfoMap.get(elem) match
          case Some(info) => info.resultCell
          case None       => module.newOnceCell[ElabConstraint, AST](solver)
      }
      val elemType = module.newOnceCell[ElabConstraint, AST](solver)

      elem match
        case CST.SeqOf(seqElems, span) if seqElems.toVector.headOption.exists { case CST.Symbol("import", _) => true; case _ => false } =>
          val elems = seqElems.toVector

          def parsedImport: Option[(JSImportKind, String, String)] = {
            // Supported forms:
            // - import x from "mod"
            // - import * as x from "mod"
            elems match
              case Vector(_, CST.Symbol("*", _), CST.Symbol("as", _), CST.Symbol(local, _), CST.Symbol("from", _), CST.StringLiteral(mod, _)) =>
                Some((JSImportKind.Namespace, local, mod))
              case Vector(_, CST.Symbol(local, _), CST.Symbol("from", _), CST.StringLiteral(mod, _)) =>
                Some((JSImportKind.Namespace, local, mod))
              case Vector(_, CST.Symbol(local, _), CST.StringLiteral(mod, _)) =>
                // Shorthand: import x "mod"
                Some((JSImportKind.Namespace, local, mod))
              case _ => None
          }

          parsedImport match
            case Some((kind, localName, modulePath)) =>
              val normalizedModule = JSImportSignature.normalizeModuleSpecifier(modulePath)
              val sig = currentCtx.jsImports
                .get(normalizedModule)
                .orElse(currentCtx.jsImports.get(modulePath))
                .getOrElse(JSImportSignature(Vector.empty, kind))

              val recordName = JSImportSignature.recordTypeNameFor(normalizedModule)
              val recordFields = JSImportSignature.freshenParams(sig.fields)

              val (recordId, recordStmtOpt, ctxWithRecord) =
                currentCtx.lookupRecord(recordName) match
                  case Some(defn) =>
                    val updated = currentCtx.updateRecord(recordName, recordFields)
                    (defn.id, None, updated)
                  case None =>
                    val recId = Uniqid.make[AST]
                    val ctorTyCell = module.newOnceCell[ElabConstraint, AST](solver)
                    val placeholderCtx = currentCtx.registerRecordPlaceholder(recordName, recId, ctorTyCell).updateRecord(recordName, recordFields)
                    val ctorType = AST.Pi(
                      Vector(Telescope(recordFields, Implicitness.Explicit)),
                      AST.RecordTypeRef(recId, recordName, span),
                      Vector.empty,
                      span
                    )
                    module.fill(solver, ctorTyCell, ctorType)
                    (recId, Some(StmtAST.Record(recId, recordName, recordFields, span)), placeholderCtx)

              val localId = Uniqid.make[AST]
              val localTyCell = module.newOnceCell[ElabConstraint, AST](solver)
              val localTy = AST.RecordTypeRef(recordId, recordName, span)
              module.fill(solver, localTyCell, localTy)
              currentCtx = ctxWithRecord.bind(localName, localId, localTyCell)

              recordStmtOpt.foreach(elaboratedElemsBuffer += _)
              elaboratedElemsBuffer += StmtAST.JSImport(localId, localName, modulePath, kind, localTy, span)
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
            case None =>
              currentCtx.reporter.report(ElabProblem.UnboundVariable("Unsupported import syntax", span))
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
              elaboratedElemsBuffer += StmtAST.ExprStmt(AST.MetaCell(HoldNotReadable(elemResult), span), span)
        case CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists { case CST.Symbol("def", _) => true; case _ => false } =>
          val elems = seqElems.toVector
          val defConstraint: ElabConstraint.Infer = ElabConstraint.Infer(elem, elemResult, elemType, currentCtx)
          val defMeta = defInfoMap(elem)
          helpers.handleDefStatement(defConstraint, elems, elem.span, defMeta.id, defMeta.tyCell, currentCtx, defInfoMap)
          val updated = defInfoMap(elem)
          // Construct a Stmt placeholder with MetaCells for body/result type
          val bodyAst = AST.MetaCell(HoldNotReadable(updated.resultCell), elem.span)
          val resTyAst = updated.resultTyCell.map(c => AST.MetaCell(HoldNotReadable(c), elem.span))
          val stmt = StmtAST.Def(updated.id, updated.name, updated.telescopes, resTyAst, bodyAst, elem.span)
          elaboratedElemsBuffer += stmt
        case CST.SeqOf(seqElems, span) if seqElems.toVector.headOption.exists { case CST.Symbol("effect", _) => true; case _ => false } =>
          val seqVec = seqElems.toVector
          val (maybeName, maybeBody) = (seqVec.lift(1), seqVec.lift(2))
          maybeName.collect { case CST.Symbol(name, _) => name } match
            case Some(name) =>
              // Declare the effect and elaborate its operation signatures (if any)
              val withEffectCtx = currentCtx.declareEffect(name)
              val effRef = withEffectCtx.lookupEffect(name).get
              val bodyCst: CST.Block = maybeBody.collect { case b: CST.Block => b }.getOrElse(CST.Block(Vector.empty, None, span))
              currentCtx = helpers.processEffectBody(bodyCst, effRef, withEffectCtx)
              // The effect declaration itself evaluates to unit
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
            case None =>
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
          elaboratedElemsBuffer += StmtAST.ExprStmt(AST.MetaCell(HoldNotReadable(elemResult), span), span)
        case CST.SeqOf(seqElems, span) if seqElems.toVector.headOption.exists { case CST.Symbol("record", _) => true; case _ => false } =>
          val recordMeta = recordInfoMap(elem)
          val fields = helpers.parseRecordFields(seqElems.toVector, currentCtx)
          val ctorType = AST.Pi(
            Vector(Telescope(fields, Implicitness.Explicit)),
            AST.RecordTypeRef(recordMeta._2, recordMeta._1, span),
            Vector.empty,
            span
          )
          module.fill(solver, recordMeta._3, ctorType)
          currentCtx = currentCtx.updateRecord(recordMeta._1, fields)
          module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
          module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
          elaboratedElemsBuffer += StmtAST.Record(recordMeta._2, recordMeta._1, fields, span)
        case CST.SeqOf(seqElems, span) if seqElems.toVector.headOption.exists {
              case CST.Symbol("enum", _) | CST.Symbol("coenum", _) => true
              case _                                               => false
            } =>
          enumInfoMap.get(elem) match
            case Some((name, enumId, typeParams, isCo)) =>
              val cases = helpers.parseEnumCases(seqElems.toVector, currentCtx, typeParams)
              val resolvedTypeParams = {
                typeParams.map { p =>
                  val resolved = helpers.normalizeTypeLikeKind(helpers.substituteSolutions(p.ty))
                  p.copy(ty = resolved, default = p.default.map(helpers.substituteSolutions))
                }
              }
              val resolvedCases = cases.map { c =>
                c.copy(params =
                  c.params.map(p => p.copy(ty = helpers.substituteSolutions(p.ty), default = p.default.map(helpers.substituteSolutions)))
                )
              }
              currentCtx = currentCtx.updateEnum(name, resolvedTypeParams, resolvedCases, isCo)
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
              if isCo then elaboratedElemsBuffer += StmtAST.Coenum(enumId, name, resolvedTypeParams, resolvedCases, span)
              else elaboratedElemsBuffer += StmtAST.Enum(enumId, name, resolvedTypeParams, resolvedCases, span)
            case None =>
              module.fill(solver, elemResult, AST.Tuple(Vector.empty, span))
              module.fill(solver, elemType, AST.TupleType(Vector.empty, span))
        case CST.SeqOf(seqElems, _) if seqElems.toVector.headOption.exists { case CST.Symbol("let", _) => true; case _ => false } =>
          val elems = seqElems.toVector
          currentCtx = helpers.handleLetStatement(elems, elem.span, currentCtx, elemResult, elemType)
          elaboratedElemsBuffer += StmtAST.ExprStmt(AST.MetaCell(HoldNotReadable(elemResult), elem.span), elem.span)
        case _ =>
          module.addConstraint(solver, ElabConstraint.Infer(elem, elemResult, elemType, currentCtx))
          elaboratedElemsBuffer += StmtAST.ExprStmt(AST.MetaCell(HoldNotReadable(elemResult), elem.span), elem.span)
    }

    val elaboratedElems = elaboratedElemsBuffer.toVector

    // Elaborate tail (or use empty tuple if None)
    val elaboratedTail = tail match
      case Some(t) =>
        val tailResult = module.newOnceCell[ElabConstraint, AST](solver)
        // Type of block is type of tail - use c.inferredTy directly
        module.addConstraint(solver, ElabConstraint.Infer(t, tailResult, inferredTyCell, currentCtx))
        AST.MetaCell(HoldNotReadable(tailResult), t.span)
      case None =>
        // Empty tail = unit value (empty tuple)
        val emptyTuple = AST.Tuple(Vector.empty, span)
        module.fill(solver, inferredTyCell, AST.TupleType(Vector.empty, None))
        emptyTuple

    // Construct block directly with MetaCells - they'll be resolved by substituteSolutions
    val block = AST.Block(elaboratedElems, elaboratedTail, span)
    module.fill(solver, resultCell, block)
    Result.Done
  }

  private def isDefStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.toVector.headOption.exists { case CST.Symbol("def", _) => true; case _ => false }
    case _ => false

  private def isLetStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.toVector.headOption.exists { case CST.Symbol("let", _) => true; case _ => false }
    case _ => false

  private def isRecordStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.toVector.headOption.exists { case CST.Symbol("record", _) => true; case _ => false }
    case _ => false

  private def isImportStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.toVector.headOption.exists { case CST.Symbol("import", _) => true; case _ => false }
    case _ => false
  private def isEnumStatement(cst: CST): Boolean = cst match
    case CST.SeqOf(elements, _) =>
      elements.toVector.headOption.exists { case CST.Symbol("enum", _) | CST.Symbol("coenum", _) => true; case _ => false }
    case _ => false
  private def extractPackageName(cst: CST): Option[String] = cst match
    case CST.SeqOf(elements, _) =>
      elements.toVector.headOption.flatMap {
        case CST.Symbol("package", _) =>
          elements.toVector.lift(1).collect { case CST.Symbol(name, _) => name.stripSuffix(";") }
        case _ => None
      }
    case _ => None
