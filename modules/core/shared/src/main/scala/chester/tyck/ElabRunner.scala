package chester.tyck

import scala.concurrent.ExecutionContext
import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, CST}
import chester.error.VectorReporter
import chester.reader.{CharReader, FileNameAndContent, ParseError, Parser, Source, Tokenizer}
import chester.utils.elab.*

/** Shared elaboration helpers usable by both tests and the LSP. */
object ElabRunner:

  private[tyck] def validateCoreType(ast: AST): Vector[ElabProblem] = {
    given coreReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()
    CoreTypeChecker.typeCheck(ast)
    coreReporter.getReports
  }

  private def inlineEscapingLetRefs(ast: AST, ty: AST): AST = {
    def inlineRefs(node: AST, subst: Map[chester.uniqid.UniqidOf[AST], AST]): AST =
      node match
        case AST.Ref(id, _, _) =>
          subst.get(id).map(inlineRefs(_, subst)).getOrElse(node)
        case AST.Type(level, span) =>
          AST.Type(inlineRefs(level, subst), span)
        case AST.TypeOmega(level, span) =>
          AST.TypeOmega(inlineRefs(level, subst), span)
        case AST.TupleType(elements, span) =>
          AST.TupleType(elements.map(inlineRefs(_, subst)), span)
        case AST.ListType(element, span) =>
          AST.ListType(inlineRefs(element, subst), span)
        case AST.Pi(telescopes, resultTy, effects, span) =>
          val newTelescopes = telescopes.map { telescope =>
            telescope.copy(params = telescope.params.map(param => param.copy(ty = inlineRefs(param.ty, subst))))
          }
          AST.Pi(newTelescopes, inlineRefs(resultTy, subst), effects, span)
        case AST.Lam(telescopes, body, span) =>
          val newTelescopes = telescopes.map { telescope =>
            telescope.copy(params = telescope.params.map(param => param.copy(ty = inlineRefs(param.ty, subst))))
          }
          AST.Lam(newTelescopes, inlineRefs(body, subst), span)
        case AST.App(func, args, implicitArgs, span) =>
          AST.App(
            inlineRefs(func, subst),
            args.map(arg => arg.copy(value = inlineRefs(arg.value, subst))),
            implicitArgs,
            span
          )
        case AST.Ann(expr, annTy, span) =>
          AST.Ann(inlineRefs(expr, subst), inlineRefs(annTy, subst), span)
        case AST.Let(id, name, annTy, value, body, span) =>
          AST.Let(id, name, annTy.map(inlineRefs(_, subst)), inlineRefs(value, subst), inlineRefs(body, subst), span)
        case AST.Block(elements, tail, span) =>
          AST.Block(elements.map(inlineStmt(_, subst)), inlineRefs(tail, subst), span)
        case AST.RecordCtor(id, name, args, span) =>
          AST.RecordCtor(id, name, args.map(inlineRefs(_, subst)), span)
        case AST.EnumCtor(enumId, caseId, enumName, caseName, args, span) =>
          AST.EnumCtor(enumId, caseId, enumName, caseName, args.map(inlineRefs(_, subst)), span)
        case AST.FieldAccess(target, field, span) =>
          AST.FieldAccess(inlineRefs(target, subst), field, span)
        case other => other

    def inlineStmt(stmt: chester.core.StmtAST, subst: Map[chester.uniqid.UniqidOf[AST], AST]): chester.core.StmtAST =
      stmt match
        case chester.core.StmtAST.ExprStmt(expr, span) =>
          chester.core.StmtAST.ExprStmt(inlineRefs(expr, subst), span)
        case chester.core.StmtAST.JSImport(id, localName, modulePath, kind, importTy, span) =>
          chester.core.StmtAST.JSImport(id, localName, modulePath, kind, inlineRefs(importTy, subst), span)
        case chester.core.StmtAST.Def(id, name, telescopes, resultTy, body, span) =>
          val newTelescopes = telescopes.map { telescope =>
            telescope.copy(params = telescope.params.map(param => param.copy(ty = inlineRefs(param.ty, subst))))
          }
          chester.core.StmtAST.Def(id, name, newTelescopes, resultTy.map(inlineRefs(_, subst)), inlineRefs(body, subst), span)
        case chester.core.StmtAST.Record(id, name, fields, span) =>
          chester.core.StmtAST.Record(id, name, fields.map(field => field.copy(ty = inlineRefs(field.ty, subst))), span)
        case chester.core.StmtAST.Enum(id, name, typeParams, cases, span) =>
          chester.core.StmtAST.Enum(
            id,
            name,
            typeParams.map(param => param.copy(ty = inlineRefs(param.ty, subst))),
            cases.map(c => c.copy(params = c.params.map(param => param.copy(ty = inlineRefs(param.ty, subst))))),
            span
          )
        case chester.core.StmtAST.Coenum(id, name, typeParams, cases, span) =>
          chester.core.StmtAST.Coenum(
            id,
            name,
            typeParams.map(param => param.copy(ty = inlineRefs(param.ty, subst))),
            cases.map(c => c.copy(params = c.params.map(param => param.copy(ty = inlineRefs(param.ty, subst))))),
            span
          )
        case chester.core.StmtAST.Pkg(name, body, span) =>
          chester.core.StmtAST.Pkg(name, inlineRefs(body, subst), span)

    def gatherEscapingTypeSubst(node: AST, subst: Map[chester.uniqid.UniqidOf[AST], AST]): Map[chester.uniqid.UniqidOf[AST], AST] =
      node match
        case AST.Block(elements, tail, _) =>
          val blockSubst = elements.foldLeft(subst) { (acc, stmt) =>
            stmt match
              case chester.core.StmtAST.ExprStmt(AST.Let(id, _, _, value, _, _), _) =>
                acc + (id -> inlineRefs(value, acc))
              case _ =>
                acc
          }
          gatherEscapingTypeSubst(tail, blockSubst)
        case AST.Let(id, _, _, value, body, _) =>
          gatherEscapingTypeSubst(body, subst + (id -> inlineRefs(value, subst)))
        case _ =>
          subst

    inlineRefs(ty, gatherEscapingTypeSubst(ast, Map.empty))
  }

  private def reportCoreTypeProblems(asts: Iterable[AST], reporter: VectorReporter[ElabProblem]): Unit =
    asts.foreach(ast => validateCoreType(ast).foreach(reporter.report))

  def elaborateExpr(input: String, ensureCoreType: Boolean = false): (Option[AST], Option[AST], Vector[ElabProblem]) = {
    val source = Source(FileNameAndContent("repl.chester", input))
    elaborateSource(source, parseAsFile = false, ensureCoreType)
  }

  def elaborateFile(input: String, ensureCoreType: Boolean = false): (Option[AST], Option[AST], Vector[ElabProblem]) = {
    val source = Source(FileNameAndContent("file.chester", input))
    elaborateSource(source, parseAsFile = true, ensureCoreType)
  }

  def elaborateModule(inputs: Seq[String], ensureCoreType: Boolean = false)(using
      ExecutionContext
  ): (Seq[Option[AST]], Seq[Option[AST]], Vector[ElabProblem]) = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val csts = inputs.map { in =>
      val source = Source(FileNameAndContent("module.chester", in))
      val parsed = for
        chars <- CharReader.read(source)
        tokens <- Tokenizer.tokenize(chars)
      yield Parser.parseFile(tokens)
      parsed.toOption.get
    }

    given module: ProceduralSolverModule.type = ProceduralSolverModule
    val results = Elaborator.elaborateModule(csts, elabReporter)
    val asts = results.map(_._1)
    val tys = results.map(_._2)

    if ensureCoreType && elabReporter.getReports.isEmpty then reportCoreTypeProblems(asts.flatten, elabReporter)

    (asts, tys, elabReporter.getReports)
  }

  def elaborateSource(source: Source, parseAsFile: Boolean, ensureCoreType: Boolean = false): (Option[AST], Option[AST], Vector[ElabProblem]) = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    given elabReporter: VectorReporter[ElabProblem] = new VectorReporter[ElabProblem]()

    val parsed = for
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    yield {
      val cst: CST = {
        if parseAsFile then Parser.parseFile(tokens)
        else Parser.parse(tokens).cst
      }

      given module: ProceduralSolverModule.type = ProceduralSolverModule

      case class ElabHandlerConf[M <: SolverModule](module: M) extends HandlerConf[ElabConstraint, M]:
        override def getHandler(constraint: ElabConstraint): Option[Handler[ElabConstraint]] =
          Some(ElabHandler)

      val solver = module.makeSolver[ElabConstraint](ElabHandlerConf(module))
      val resultCell = module.newOnceCell[ElabConstraint, AST](solver)
      val typeCell = module.newOnceCell[ElabConstraint, AST](solver)

      module.addConstraint(solver, ElabConstraint.Infer(cst, resultCell, typeCell, ElabContext(Map.empty, Map.empty, reporter = elabReporter)))
      module.run(solver)

      val result = module.readStable(solver, resultCell)
      val ty = module.readStable(solver, typeCell)
      val zonkedResult = result.map(r => substituteSolutions(r)(using module, solver))
      val zonkedTy = ty.map(t => substituteSolutions(t)(using module, solver))
      val stabilizedTy = (for
        ast <- zonkedResult
        inferredTy <- zonkedTy
      yield inlineEscapingLetRefs(ast, inferredTy)).orElse(zonkedTy)

      if ensureCoreType && elabReporter.getReports.isEmpty then reportCoreTypeProblems(zonkedResult, elabReporter)

      (zonkedResult, stabilizedTy)
    }

    parsed match
      case Right((ast, ty)) => (ast, ty, elabReporter.getReports)
      case Left(err)        => (None, None, Vector(ElabProblem.UnboundVariable(err.toString, None)))
  }
