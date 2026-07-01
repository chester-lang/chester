package chester

import chester.syntax.*
import cats.data.NonEmptyVector
import scala.collection.mutable.ArrayBuffer

class GoCompiler {
  private def isSymbol(cst: CST, name: String): Boolean = cst match {
    case CST.Symbol(n, _) => n == name
    case _ => false
  }

  private def getSymbolName(cst: CST): Option[String] = cst match {
    case CST.Symbol(n, _) => Some(n)
    case _ => None
  }

  private def isBinaryOp(cst: CST): Boolean = cst match {
    case CST.Symbol(op, _) => Set("+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=").contains(op)
    case _ => false
  }

  def collectRecordNames(cst: CST): Set[String] = cst match {
    case CST.Block(elements, tail, _) =>
      elements.flatMap(collectRecordNames).toSet ++ tail.map(collectRecordNames).getOrElse(Set.empty)
    case CST.SeqOf(elems, _) =>
      val elList = elems.toVector
      if (elList.length >= 2 && isSymbol(elList(0), "record")) {
        getSymbolName(elList(1)).toSet
      } else {
        elList.flatMap(collectRecordNames).toSet
      }
    case _ => Set.empty
  }

  def lowerType(cst: CST): GoType = cst match {
    case CST.Symbol("String", span)  => GoType.Named("string", span)
    case CST.Symbol("Integer", span) => GoType.Named("int", span)
    case CST.Symbol("Natural", span) => GoType.Named("uint", span)
    case CST.Symbol("Bool", span)    => GoType.Named("bool", span)
    case CST.Symbol("Unit", span)    => GoType.Struct(Vector.empty, span)
    case CST.Symbol(name, span)      => GoType.Named(name, span)
    case _                           => GoType.Named("any", cst.span)
  }

  def parseField(cst: CST): GoField = cst match {
    case CST.SeqOf(elements, span) if elements.length >= 3 && isSymbol(elements.toVector(1), ":") =>
      val name = getSymbolName(elements.toVector(0)).getOrElse("field")
      val typeCst = elements.toVector(2)
      GoField(Vector(name), lowerType(typeCst), None, isEmbedded = false, isVariadic = false, span)
    case other =>
      val name = getSymbolName(other).getOrElse("field")
      GoField(Vector(name), lowerType(other), None, isEmbedded = false, isVariadic = false, other.span)
  }

  def lowerExpr(cst: CST, recordNames: Set[String]): GoAST = {
    parseToAST(cst, recordNames)
  }

  private def parseToAST(cst: CST, recordNames: Set[String]): GoAST = cst match {
    case CST.IntegerLiteral(value, span) => GoAST.IntLiteral(value.toString, span)
    case CST.StringLiteral(value, span)  => GoAST.StringLiteral(value, span)
    case CST.Symbol("true", span)        => GoAST.BoolLiteral(true, span)
    case CST.Symbol("false", span)       => GoAST.BoolLiteral(false, span)
    case CST.Symbol(name, span)          => GoAST.Identifier(name, span)
    case CST.Tuple(elements, span) =>
      if (elements.isEmpty) {
        GoAST.CompositeLiteral(GoType.Struct(Vector.empty, span), Vector.empty, span)
      } else {
        GoAST.CompositeLiteral(
          GoType.Slice(GoType.Named("any", span), span),
          elements.map(e => GoCompositeElement(None, parseToAST(e, recordNames), e.span)),
          span
        )
      }
    case CST.SeqOf(elems, _) =>
      reconstructExpr(elems.toVector, recordNames)
    case _ => GoAST.NilLiteral(cst.span)
  }

  private def reconstructExpr(elems: Seq[CST], recordNames: Set[String]): GoAST = {
    val grouped = groupCallsAndTuples(elems, recordNames)
    if (grouped.isEmpty) return GoAST.NilLiteral(None)
    if (grouped.length == 1) return grouped.head

    val exprs = grouped.toBuffer
    var i = 1
    while (i < exprs.length - 1) {
      exprs(i) match {
        case GoAST.Identifier(".", span) =>
          val left = exprs(i - 1)
          val right = exprs(i + 1) match {
            case GoAST.Identifier(name, _) => name
            case _ => "field"
          }
          val selector = GoAST.Selector(left, right, span)
          exprs.remove(i - 1, 3)
          exprs.insert(i - 1, selector)
        case _ =>
          i += 1
      }
    }

    def processOps(ops: Set[String]): Unit = {
      var idx = 1
      while (idx < exprs.length - 1) {
        exprs(idx) match {
          case GoAST.Identifier(op, span) if ops.contains(op) =>
            val left = exprs(idx - 1)
            val right = exprs(idx + 1)
            val binary = GoAST.Binary(left, op, right, span)
            exprs.remove(idx - 1, 3)
            exprs.insert(idx - 1, binary)
          case _ =>
            idx += 1
        }
      }
    }

    processOps(Set("*", "/"))
    processOps(Set("+", "-"))
    processOps(Set("==", "!=", "<", ">", "<=", ">="))

    exprs.head
  }

  private def groupCallsAndTuples(elems: Seq[CST], recordNames: Set[String]): Seq[GoAST] = {
    val result = ArrayBuffer.empty[GoAST]
    var i = 0
    val len = elems.length
    while (i < len) {
      val current = elems(i)
      if (i + 1 < len && elems(i + 1).isInstanceOf[CST.Tuple]) {
        val argsTuple = elems(i + 1).asInstanceOf[CST.Tuple]
        val args = argsTuple.elements.map(e => parseToAST(e, recordNames))
        val currentAST = parseToAST(current, recordNames)
        
        val funcName = current match {
          case CST.Symbol(n, _) => n
          case _ => ""
        }
        if (recordNames.contains(funcName)) {
          val compositeElems = args.map(arg => GoCompositeElement(None, arg, None))
          result += GoAST.CompositeLiteral(GoType.Named(funcName, current.span), compositeElems, current.span)
        } else {
          result += GoAST.Call(currentAST, args, current.span)
        }
        i += 2
      } else {
        result += parseToAST(current, recordNames)
        i += 1
      }
    }
    result.toSeq
  }

  def lowerStmt(cst: CST, recordNames: Set[String]): Vector[GoAST] = cst match {
    case CST.SeqOf(elems, span) =>
      val list = elems.toVector
      if (list.length >= 2 && isSymbol(list(0), "record")) {
        lowerRecordDecl(list, span).toVector
      } else if (list.length >= 2 && isSymbol(list(0), "def")) {
        lowerFuncDecl(list, span, recordNames).toVector
      } else if (list.length >= 4 && isSymbol(list(0), "let") && isSymbol(list(2), "=")) {
        val name = getSymbolName(list(1)).getOrElse("x")
        val value = lowerExpr(list(3), recordNames)
        Vector(GoAST.Assign(Vector(GoAST.Identifier(name, span)), Vector(value), ":=", span))
      } else {
        Vector(GoAST.ExprStmt(lowerExpr(cst, recordNames), span))
      }
    case other =>
      Vector(GoAST.ExprStmt(lowerExpr(other, recordNames), other.span))
  }

  private def lowerRecordDecl(elems: Vector[CST], span: Option[Span]): Option[GoAST] = {
    for {
      name <- getSymbolName(elems(1))
      fieldsCst <- Some(elems(2))
    } yield {
      val fields = fieldsCst match {
        case CST.Tuple(fElems, _) => fElems.map(parseField)
        case _ => Vector.empty
      }
      val typeSpec = GoTypeSpec(name, Vector.empty, GoType.Struct(fields, span), isAlias = false, span)
      GoAST.TypeDecl(Vector(typeSpec), span)
    }
  }

  private def lowerFuncDecl(elems: Vector[CST], span: Option[Span], recordNames: Set[String]): Option[GoAST] = {
    if (elems.length >= 3 && isSymbol(elems(0), "def")) {
      for {
        name <- getSymbolName(elems(1))
        paramsTuple <- Some(elems(2))
      } yield {
        val params = paramsTuple match {
          case CST.Tuple(pElems, _) => pElems.map(parseField)
          case _ => Vector.empty
        }
        
        val eqIndex = elems.indexWhere(isSymbol(_, "="))
        val bodyCst = elems.drop(eqIndex + 1).head
        val resultType = if (eqIndex > 3 && isSymbol(elems(eqIndex - 2), ":")) {
          Vector(GoField(Vector.empty, lowerType(elems(eqIndex - 1)), None, isEmbedded = false, isVariadic = false, elems(eqIndex - 1).span))
        } else Vector.empty
        
        val body = lowerFunctionBody(bodyCst, recordNames)
        GoAST.FuncDecl(name, Vector.empty, params, resultType, Some(body), None, span)
      }
    } else None
  }

  private def lowerFunctionBody(bodyCst: CST, recordNames: Set[String]): GoAST.Block = {
    bodyCst match {
      case CST.Block(elements, tail, span) =>
        val stmts = elements.flatMap(e => lowerStmt(e, recordNames))
        val tailStmt = tail.map {
          case CST.Tuple(elems, tSpan) if elems.isEmpty =>
            GoAST.ExprStmt(GoAST.CompositeLiteral(GoType.Struct(Vector.empty, tSpan), Vector.empty, tSpan), tSpan)
          case t =>
            GoAST.Return(Vector(lowerExpr(t, recordNames)), t.span)
        }.toVector
        GoAST.Block(stmts ++ tailStmt, span)
      case other =>
        val expr = lowerExpr(other, recordNames)
        val stmt = expr match {
          case GoAST.CompositeLiteral(GoType.Struct(fields, _), _, _) if fields.isEmpty =>
            GoAST.ExprStmt(expr, other.span)
          case _ =>
            GoAST.Return(Vector(expr), other.span)
        }
        GoAST.Block(Vector(stmt), other.span)
    }
  }

  def compile(cst: CST, packageName: String = "main"): GoAST.File = {
    val recordNames = collectRecordNames(cst)
    val decls = ArrayBuffer.empty[GoAST]
    val execStmts = ArrayBuffer.empty[GoAST]

    cst match {
      case CST.Block(elements, tail, span) =>
        elements.foreach { elem =>
          val lowered = lowerStmt(elem, recordNames)
          lowered.foreach {
            case d: GoAST.TypeDecl => decls += d
            case d: GoAST.FuncDecl => decls += d
            case other => execStmts += other
          }
        }
        tail.foreach { t =>
          val lowered = lowerExpr(t, recordNames)
          val tailIsUnit = lowered match {
            case GoAST.CompositeLiteral(GoType.Struct(fields, _), _, _) if fields.isEmpty => true
            case _ => false
          }
          if (!tailIsUnit) {
            val fmtPrintln = GoAST.Selector(GoAST.Identifier("fmt", t.span), "Println", t.span)
            execStmts += GoAST.ExprStmt(GoAST.Call(fmtPrintln, Vector(lowered), t.span), t.span)
          }
        }
      case other =>
        val lowered = lowerExpr(other, recordNames)
        val tailIsUnit = lowered match {
          case GoAST.CompositeLiteral(GoType.Struct(fields, _), _, _) if fields.isEmpty => true
          case _ => false
        }
        if (!tailIsUnit) {
          val fmtPrintln = GoAST.Selector(GoAST.Identifier("fmt", other.span), "Println", other.span)
          execStmts += GoAST.ExprStmt(GoAST.Call(fmtPrintln, Vector(lowered), other.span), other.span)
        }
    }

    val imports = ArrayBuffer.empty[GoImportSpec]
    if (execStmts.nonEmpty) {
      imports += GoImportSpec(None, "fmt", cst.span)
      val mainBody = GoAST.Block(execStmts.toVector, cst.span)
      val mainFunc = GoAST.FuncDecl("main", Vector.empty, Vector.empty, Vector.empty, Some(mainBody), None, cst.span)
      decls += mainFunc
    }

    GoAST.File(packageName, imports.toVector, decls.toVector, cst.span)
  }
}

object GoCompiler:
  def compile(input: String, packageName: String = "main"): String = {
    given reporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val source = Source(FileNameAndContent("input.chester", input))
    val cst = (for {
      chars <- CharReader.read(source)
      tokens <- Tokenizer.tokenize(chars)
    } yield Parser.parseFile(tokens)).getOrElse(throw new RuntimeException("Parse failed"))
    
    val compiler = new GoCompiler()
    val file = compiler.compile(cst, packageName)
    given DocConf = DocConf.Default
    file.toDoc.layout(0)
  }
