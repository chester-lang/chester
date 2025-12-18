package chester.core

import scala.language.experimental.genericNumberLiterals

import chester.error.{Span, SpanOptional}
import chester.utils.doc.{*, given}
import chester.utils.doc.Docs.*
import upickle.default.*
import cats.data.NonEmptyVector
import chester.utils.{*, given}
import chester.utils.{HoldNotReadable, holdNotReadableRW}
import chester.uniqid.{ContainsUniqid, Uniqid, UniqidCollector, UniqidOf, UniqidReplacer, rwUniqIDOf, given}

enum Implicitness derives ReadWriter:
  case Implicit
  case Explicit

enum Coeffect derives ReadWriter:
  case Zero
  case One
  case Unrestricted

case class Param(
    id: UniqidOf[AST],
    name: String,
    ty: AST,
    implicitness: Implicitness = Implicitness.Explicit,
    default: Option[AST] = None,
    coeffect: Coeffect = Coeffect.Unrestricted
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit = {
    collector(id)
    ty.collectUniqids(collector)
    default.foreach(_.collectUniqids(collector))
  }

  def mapUniqids(mapper: UniqidReplacer): Param =
    Param(mapper(id), name, ty.mapUniqids(mapper), implicitness, default.map(_.mapUniqids(mapper)), coeffect)

/** Telescope: a sequence of parameters with the same implicitness */
case class Telescope(
    params: Vector[Param],
    implicitness: Implicitness
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit =
    params.foreach(_.collectUniqids(collector))

  def mapUniqids(mapper: UniqidReplacer): Telescope =
    Telescope(params.map(_.mapUniqids(mapper)), implicitness)

case class Arg(
    value: AST,
    implicitness: Implicitness = Implicitness.Explicit
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit =
    value.collectUniqids(collector)

  def mapUniqids(mapper: UniqidReplacer): Arg =
    Arg(value.mapUniqids(mapper), implicitness)

enum BuiltinEffect derives ReadWriter:
  case Io

  def name: String = this match
    case Io => "io"

enum EffectRef derives ReadWriter:
  case Builtin(effect: BuiltinEffect)
  case User(id: UniqidOf[AST], effName: String)

  def name: String = this match
    case Builtin(effect) => effect.name
    case User(_, n)      => n

  def collectUniqids(collector: UniqidCollector): Unit = this match
    case Builtin(_)  => ()
    case User(id, _) => collector(id)

  def mapUniqids(mapper: UniqidReplacer): EffectRef = this match
    case Builtin(effect) => this
    case User(id, n)     => User(mapper(id), n)

case class EnumCase(
    id: UniqidOf[AST],
    name: String,
    params: Vector[Param]
) derives ReadWriter:
  def collectUniqids(collector: UniqidCollector): Unit = {
    collector(id)
    params.foreach(_.collectUniqids(collector))
  }

  def mapUniqids(mapper: UniqidReplacer): EnumCase =
    EnumCase(mapper(id), name, params.map(_.mapUniqids(mapper)))

// Ensure HoldNotReadable ReadWriter is in scope for MetaCell
given [T]: ReadWriter[HoldNotReadable[T]] = holdNotReadableRW.asInstanceOf[ReadWriter[HoldNotReadable[T]]]

enum StmtAST(val span: Option[Span]) extends ToDoc with ContainsUniqid with SpanOptional derives ReadWriter:
  case ExprStmt(expr: AST, override val span: Option[Span]) extends StmtAST(span)
  case JSImport(
      id: UniqidOf[AST],
      localName: String,
      modulePath: String,
      kind: JSImportKind,
      ty: AST,
      override val span: Option[Span]
  ) extends StmtAST(span)
  case Def(id: UniqidOf[AST], name: String, telescopes: Vector[Telescope], resultTy: Option[AST], body: AST, override val span: Option[Span])
      extends StmtAST(span)
  case Record(id: UniqidOf[AST], name: String, fields: Vector[Param], override val span: Option[Span]) extends StmtAST(span)
  case Enum(id: UniqidOf[AST], name: String, typeParams: Vector[Param], cases: Vector[EnumCase], override val span: Option[Span])
      extends StmtAST(span)
  case Coenum(id: UniqidOf[AST], name: String, typeParams: Vector[Param], cases: Vector[EnumCase], override val span: Option[Span])
      extends StmtAST(span)
  case Pkg(name: String, body: AST, override val span: Option[Span]) extends StmtAST(span)

  def toDoc(using options: DocConf): Doc = this match
    case StmtAST.ExprStmt(expr, _) => expr.toDoc
    case StmtAST.JSImport(_, localName, modulePath, kind, _, _) =>
      val importDoc = kind match
        case JSImportKind.Namespace => text("* as ") <> text(localName)
        case JSImportKind.Default   => text(localName)
      text("import") <+> importDoc <+> text("from") <+> text("\"") <> text(modulePath) <> text("\"")
    case StmtAST.Def(_, name, telescopes, resultTy, body, _) =>
      val telescopeDocs = telescopes.map { tel =>
        val bracket = if tel.implicitness == Implicitness.Implicit then (brackets, brackets) else (parens, parens)
        val paramsDoc = hsep(
          tel.params.map { p =>
            val coeffDoc = p.coeffect match
              case Coeffect.One          => text("1") <+> empty
              case Coeffect.Zero         => text("0") <+> empty
              case Coeffect.Unrestricted => empty
            coeffDoc <> text(p.name) <> text(":") <+> p.ty.toDoc
          },
          `,` <+> empty
        )
        bracket._1(paramsDoc)
      }
      val tyDoc = resultTy.map(t => text(":") <+> t.toDoc).getOrElse(empty)
      text("def") <+> text(name) <> hsep(telescopeDocs, empty) <+> tyDoc <+> text("=") <+> body.toDoc
    case StmtAST.Record(_, name, fields, _) =>
      val paramsDoc = hsep(
        fields.map { p =>
          val coeffDoc = p.coeffect match
            case Coeffect.One          => text("1") <+> empty
            case Coeffect.Zero         => text("0") <+> empty
            case Coeffect.Unrestricted => empty
          coeffDoc <> text(p.name) <> text(":") <+> p.ty.toDoc
        },
        `,` <+> empty
      )
      text("record") <+> text(name) <> parens(paramsDoc)
    case StmtAST.Enum(_, name, typeParams, cases, _) =>
      val typeParamsDoc = {
        if typeParams.isEmpty then empty
        else {
          parens(
            hsep(
              typeParams.map { p =>
                val coeffDoc = p.coeffect match
                  case Coeffect.One          => text("1") <+> empty
                  case Coeffect.Zero         => text("0") <+> empty
                  case Coeffect.Unrestricted => empty
                coeffDoc <> text(p.name) <> text(":") <+> p.ty.toDoc
              },
              `,` <+> empty
            )
          )
        }
      }
      val caseDocs = cases.map { c =>
        val paramsDoc = hsep(
          c.params.map { p =>
            val coeffDoc = p.coeffect match
              case Coeffect.One          => text("1") <+> empty
              case Coeffect.Zero         => text("0") <+> empty
              case Coeffect.Unrestricted => empty
            coeffDoc <> text(p.name) <> text(":") <+> p.ty.toDoc
          },
          `,` <+> empty
        )
        val paramsRendered = if paramsDoc == empty then empty else parens(paramsDoc)
        text("case") <+> text(c.name) <> paramsRendered
      }
      val bodyDoc = {
        if caseDocs.isEmpty then empty
        else line <> ssep(caseDocs, `;` <> line).indented() <> line
      }
      text("enum") <+> text(name) <> typeParamsDoc <+> braces(bodyDoc)
    case StmtAST.Coenum(_, name, typeParams, cases, _) =>
      val typeParamsDoc = {
        if typeParams.isEmpty then empty
        else {
          parens(
            hsep(
              typeParams.map { p =>
                val coeffDoc = p.coeffect match
                  case Coeffect.One          => text("1") <+> empty
                  case Coeffect.Zero         => text("0") <+> empty
                  case Coeffect.Unrestricted => empty
                coeffDoc <> text(p.name) <> text(":") <+> p.ty.toDoc
              },
              `,` <+> empty
            )
          )
        }
      }
      val caseDocs = cases.map { c =>
        val paramsDoc = hsep(
          c.params.map { p =>
            val coeffDoc = p.coeffect match
              case Coeffect.One          => text("1") <+> empty
              case Coeffect.Zero         => text("0") <+> empty
              case Coeffect.Unrestricted => empty
            coeffDoc <> text(p.name) <> text(":") <+> p.ty.toDoc
          },
          `,` <+> empty
        )
        val paramsRendered = if paramsDoc == empty then empty else parens(paramsDoc)
        text("case") <+> text(c.name) <> paramsRendered
      }
      val bodyDoc = {
        if caseDocs.isEmpty then empty
        else line <> ssep(caseDocs, `;` <> line).indented() <> line
      }
      text("coenum") <+> text(name) <> typeParamsDoc <+> braces(bodyDoc)
    case StmtAST.Pkg(name, body, _) =>
      text("package") <+> text(name) <@@> body.toDoc.indented()

  def collectUniqids(collector: UniqidCollector): Unit = this match
    case StmtAST.ExprStmt(expr, _) =>
      expr.collectUniqids(collector)
    case StmtAST.JSImport(id, _, _, _, ty, _) =>
      collector(id)
      ty.collectUniqids(collector)
    case StmtAST.Def(id, _, telescopes, resultTy, body, _) =>
      collector(id)
      telescopes.foreach(_.collectUniqids(collector))
      resultTy.foreach(_.collectUniqids(collector))
      body.collectUniqids(collector)
    case StmtAST.Record(id, _, fields, _) =>
      collector(id)
      fields.foreach(_.collectUniqids(collector))
    case StmtAST.Enum(id, _, typeParams, cases, _) =>
      collector(id)
      typeParams.foreach(_.collectUniqids(collector))
      cases.foreach(_.collectUniqids(collector))
    case StmtAST.Coenum(id, _, typeParams, cases, _) =>
      collector(id)
      typeParams.foreach(_.collectUniqids(collector))
      cases.foreach(_.collectUniqids(collector))
    case StmtAST.Pkg(_, body, _) =>
      body.collectUniqids(collector)

  def mapUniqids(mapper: UniqidReplacer): StmtAST = this match
    case StmtAST.ExprStmt(expr, span) =>
      StmtAST.ExprStmt(expr.mapUniqids(mapper), span)
    case StmtAST.JSImport(id, localName, modulePath, kind, ty, span) =>
      StmtAST.JSImport(mapper(id), localName, modulePath, kind, ty.mapUniqids(mapper), span)
    case StmtAST.Def(id, name, telescopes, resultTy, body, span) =>
      StmtAST.Def(
        mapper(id),
        name,
        telescopes.map(_.mapUniqids(mapper)),
        resultTy.map(_.mapUniqids(mapper)),
        body.mapUniqids(mapper),
        span
      )
    case StmtAST.Record(id, name, fields, span) =>
      StmtAST.Record(
        mapper(id),
        name,
        fields.map(_.mapUniqids(mapper)),
        span
      )
    case StmtAST.Enum(id, name, typeParams, cases, span) =>
      StmtAST.Enum(
        mapper(id),
        name,
        typeParams.map(_.mapUniqids(mapper)),
        cases.map(_.mapUniqids(mapper)),
        span
      )
    case StmtAST.Coenum(id, name, typeParams, cases, span) =>
      StmtAST.Coenum(
        mapper(id),
        name,
        typeParams.map(_.mapUniqids(mapper)),
        cases.map(_.mapUniqids(mapper)),
        span
      )
    case StmtAST.Pkg(name, body, span) =>
      StmtAST.Pkg(name, body.mapUniqids(mapper), span)

enum JSImportKind derives ReadWriter:
  case Namespace
  case Default

enum AST(val span: Option[Span]) extends ToDoc with ContainsUniqid with SpanOptional derives ReadWriter:
  case Ref(id: UniqidOf[AST], name: String, override val span: Option[Span]) extends AST(span)
  case Tuple(elements: Vector[AST], override val span: Option[Span]) extends AST(span)
  case ListLit(elements: Vector[AST], override val span: Option[Span]) extends AST(span)
  case Block(elements: Vector[StmtAST], tail: AST, override val span: Option[Span]) extends AST(span)
  case StringLit(value: String, override val span: Option[Span]) extends AST(span)
  case IntLit(value: BigInt, override val span: Option[Span]) extends AST(span)
  case NaturalLit(value: BigInt, override val span: Option[Span]) extends AST(span)
  case LevelLit(value: BigInt, override val span: Option[Span]) extends AST(span)
  case Type(level: AST, override val span: Option[Span]) extends AST(span)
  case TypeOmega(level: AST, override val span: Option[Span]) extends AST(span)
  case AnyType(override val span: Option[Span]) extends AST(span)
  case StringType(override val span: Option[Span]) extends AST(span)
  case NaturalType(override val span: Option[Span]) extends AST(span)
  case IntegerType(override val span: Option[Span]) extends AST(span)
  case LevelType(override val span: Option[Span]) extends AST(span)
  case TupleType(elements: Vector[AST], override val span: Option[Span]) extends AST(span)
  case ListType(element: AST, override val span: Option[Span]) extends AST(span)
  case Pi(telescopes: Vector[Telescope], resultTy: AST, effects: Vector[EffectRef], override val span: Option[Span]) extends AST(span)
  case Lam(telescopes: Vector[Telescope], body: AST, override val span: Option[Span]) extends AST(span)
  case App(func: AST, args: Vector[Arg], implicitArgs: Boolean, override val span: Option[Span]) extends AST(span)
  case Let(id: UniqidOf[AST], name: String, ty: Option[AST], value: AST, body: AST, override val span: Option[Span]) extends AST(span)
  case Ann(expr: AST, ty: AST, override val span: Option[Span]) extends AST(span)
  case RecordTypeRef(id: UniqidOf[AST], name: String, override val span: Option[Span]) extends AST(span)
  case RecordCtor(id: UniqidOf[AST], name: String, args: Vector[AST], override val span: Option[Span]) extends AST(span)
  case FieldAccess(target: AST, field: String, override val span: Option[Span]) extends AST(span)
  case EnumTypeRef(id: UniqidOf[AST], name: String, override val span: Option[Span]) extends AST(span)
  case EnumCaseRef(enumId: UniqidOf[AST], caseId: UniqidOf[AST], enumName: String, caseName: String, override val span: Option[Span])
      extends AST(span)
  case EnumCtor(enumId: UniqidOf[AST], caseId: UniqidOf[AST], enumName: String, caseName: String, args: Vector[AST], override val span: Option[Span])
      extends AST(span)
  case MetaCell(cell: HoldNotReadable[chester.utils.elab.CellRW[AST]], override val span: Option[Span]) extends AST(span)

  def toDoc(using options: DocConf): Doc = this match
    case AST.Ref(id, name, _) =>
      text(name)
    case AST.Tuple(elements, _) =>
      parens(hsep(elements.map(_.toDoc), `,` <+> empty))
    case AST.ListLit(elements, _) =>
      brackets(hsep(elements.map(_.toDoc), `,` <+> empty))
    case AST.Block(elements, tail, _) =>
      val elemsDoc = if elements.isEmpty then empty else ssep(elements.map(_.toDoc), `;` <> line) <> `;` <> line
      braces(line <> (elemsDoc <> tail.toDoc).indented() <> line)
    case AST.StringLit(value, _) =>
      text("\"") <> text(value) <> text("\"")
    case AST.IntLit(value, _) =>
      text(value.toString)
    case AST.NaturalLit(value, _) =>
      text(value.toString)
    case AST.LevelLit(value, _) =>
      text(value.toString)
    case AST.Type(level, _) =>
      text("Type") <> parens(level.toDoc)
    case AST.TypeOmega(level, _) =>
      text("Typeω") <> parens(level.toDoc)
    case AST.AnyType(_) =>
      text("Any")
    case AST.LevelType(_) =>
      text("Level")
    case AST.StringType(_) =>
      text("String")
    case AST.NaturalType(_) =>
      text("Natural")
    case AST.IntegerType(_) =>
      text("Integer")
    case AST.TupleType(elements, _) =>
      parens(hsep(elements.map(_.toDoc), `,` <+> empty))
    case AST.ListType(element, _) =>
      text("List") <> brackets(element.toDoc)
    case AST.Pi(telescopes, resultTy, effects, _) =>
      val telescopeDocs = telescopes.map { tel =>
        val bracket = if tel.implicitness == Implicitness.Implicit then (brackets, brackets) else (parens, parens)
        val paramsDoc = hsep(
          tel.params.map { p =>
            text(p.name) <> text(":") <+> p.ty.toDoc <>
              p.default.map(d => text(" = ") <> d.toDoc).getOrElse(empty)
          },
          `,` <+> empty
        )
        bracket._1(paramsDoc)
      }
      val effDoc = {
        if effects.isEmpty then empty
        else text(" / ") <> brackets(hsep(effects.map(e => text(e.name)), `,` <+> empty))
      }
      hsep(telescopeDocs, empty) <+> text("->") <+> resultTy.toDoc <> effDoc
    case AST.Lam(telescopes, body, _) =>
      val telescopeDocs = telescopes.map { tel =>
        val bracket = if tel.implicitness == Implicitness.Implicit then (brackets, brackets) else (parens, parens)
        val paramsDoc = hsep(
          tel.params.map { p =>
            text(p.name) <> text(":") <+> p.ty.toDoc <>
              p.default.map(d => text(" = ") <> d.toDoc).getOrElse(empty)
          },
          `,` <+> empty
        )
        bracket._1(paramsDoc)
      }
      text("λ") <> hsep(telescopeDocs, empty) <+> text("=>") <+> body.toDoc
    case AST.App(func, args, implicitArgs, _) =>
      val bracket = if implicitArgs then (brackets, brackets) else (parens, parens)
      val argsDoc = hsep(args.map(_.value.toDoc), `,` <+> empty)
      func.toDoc <> bracket._1(argsDoc)
    case AST.Let(id, name, ty, value, body, _) =>
      val tyDoc = ty.map(t => text(":") <+> t.toDoc).getOrElse(empty)
      text("let") <+> text(name) <+> tyDoc <+> text("=") <+> value.toDoc <+> text("in") <@@> body.toDoc.indented()
    case AST.Ann(expr, ty, _) =>
      parens(expr.toDoc <+> text(":") <+> ty.toDoc)
    case AST.RecordTypeRef(_, name, _) =>
      text(name) <> text(".t")
    case AST.RecordCtor(_, name, args, _) =>
      // Desugared constructor call: Vec2d(a, b) => Vec2d.apply(a, b)
      text(name) <> text(".apply") <> parens(hsep(args.map(_.toDoc), `,` <+> empty))
    case AST.FieldAccess(target, field, _) =>
      target.toDoc <> text(".") <> text(field)
    case AST.EnumTypeRef(_, name, _) =>
      text(name) <> text(".t")
    case AST.EnumCaseRef(_, _, enumName, caseName, _) =>
      text(enumName) <> text(".") <> text(caseName)
    case AST.EnumCtor(_, _, enumName, caseName, args, _) =>
      val argsDoc = if args.isEmpty then empty else parens(hsep(args.map(_.toDoc), `,` <+> empty))
      text(enumName) <> text(".") <> text(caseName) <> argsDoc
    case AST.MetaCell(cell, _) =>
      text("?") <> text(cell.toString)

  def collectUniqids(collector: UniqidCollector): Unit = this match
    case AST.Ref(id, _, _) =>
      collector(id)
    case AST.Tuple(elements, _) =>
      elements.foreach(_.collectUniqids(collector))
    case AST.ListLit(elements, _) =>
      elements.foreach(_.collectUniqids(collector))
    case AST.Block(elements, tail, _) =>
      elements.foreach(_.collectUniqids(collector))
      tail.collectUniqids(collector)
    case AST.StringLit(_, _) =>
      ()
    case AST.IntLit(_, _) =>
      ()
    case AST.NaturalLit(_, _) =>
      ()
    case AST.LevelLit(_, _) =>
      ()
    case AST.Type(level, _) =>
      level.collectUniqids(collector)
    case AST.TypeOmega(level, _) =>
      level.collectUniqids(collector)
    case AST.AnyType(_) =>
      ()
    case AST.LevelType(_) =>
      ()
    case AST.StringType(_) =>
      ()
    case AST.NaturalType(_) =>
      ()
    case AST.IntegerType(_) =>
      ()
    case AST.TupleType(elements, _) =>
      elements.foreach(_.collectUniqids(collector))
    case AST.ListType(element, _) =>
      element.collectUniqids(collector)
    case AST.Pi(telescopes, resultTy, effects, _) =>
      telescopes.foreach(_.collectUniqids(collector))
      resultTy.collectUniqids(collector)
      effects.foreach(_.collectUniqids(collector))
    case AST.Lam(telescopes, body, _) =>
      telescopes.foreach(_.collectUniqids(collector))
      body.collectUniqids(collector)
    case AST.App(func, args, _, _) =>
      func.collectUniqids(collector)
      args.foreach(_.collectUniqids(collector))
    case AST.Let(id, _, ty, value, body, _) =>
      collector(id)
      ty.foreach(_.collectUniqids(collector))
      value.collectUniqids(collector)
      body.collectUniqids(collector)
    case AST.Ann(expr, ty, _) =>
      expr.collectUniqids(collector)
      ty.collectUniqids(collector)
    case AST.RecordTypeRef(id, _, _) =>
      collector(id)
    case AST.RecordCtor(id, _, args, _) =>
      collector(id)
      args.foreach(_.collectUniqids(collector))
    case AST.FieldAccess(target, _, _) =>
      target.collectUniqids(collector)
    case AST.EnumTypeRef(id, _, _) =>
      collector(id)
    case AST.EnumCaseRef(enumId, caseId, _, _, _) =>
      collector(enumId)
      collector(caseId)
    case AST.EnumCtor(enumId, caseId, _, _, args, _) =>
      collector(enumId)
      collector(caseId)
      args.foreach(_.collectUniqids(collector))
    case AST.MetaCell(_, _) =>
      ()

  def mapUniqids(mapper: UniqidReplacer): AST = this match
    case AST.Ref(id, name, span) =>
      AST.Ref(mapper(id), name, span)
    case AST.Tuple(elements, span) =>
      AST.Tuple(elements.map(_.mapUniqids(mapper)), span)
    case AST.ListLit(elements, span) =>
      AST.ListLit(elements.map(_.mapUniqids(mapper)), span)
    case AST.Block(elements, tail, span) =>
      AST.Block(elements.map(_.mapUniqids(mapper)), tail.mapUniqids(mapper), span)
    case AST.StringLit(value, span) =>
      AST.StringLit(value, span)
    case AST.IntLit(value, span) =>
      AST.IntLit(value, span)
    case AST.NaturalLit(value, span) =>
      AST.NaturalLit(value, span)
    case AST.LevelLit(value, span) =>
      AST.LevelLit(value, span)
    case AST.Type(level, span) =>
      AST.Type(level.mapUniqids(mapper), span)
    case AST.TypeOmega(level, span) =>
      AST.TypeOmega(level.mapUniqids(mapper), span)
    case AST.AnyType(span) =>
      AST.AnyType(span)
    case AST.LevelType(span) =>
      AST.LevelType(span)
    case AST.StringType(span) =>
      AST.StringType(span)
    case AST.NaturalType(span) =>
      AST.NaturalType(span)
    case AST.IntegerType(span) =>
      AST.IntegerType(span)
    case AST.TupleType(elements, span) =>
      AST.TupleType(elements.map(_.mapUniqids(mapper)), span)
    case AST.ListType(element, span) =>
      AST.ListType(element.mapUniqids(mapper), span)
    case AST.Pi(telescopes, resultTy, effects, span) =>
      AST.Pi(
        telescopes.map(_.mapUniqids(mapper)),
        resultTy.mapUniqids(mapper),
        effects.map(_.mapUniqids(mapper)),
        span
      )
    case AST.Lam(telescopes, body, span) =>
      AST.Lam(
        telescopes.map(_.mapUniqids(mapper)),
        body.mapUniqids(mapper),
        span
      )
    case AST.App(func, args, implicitArgs, span) =>
      AST.App(
        func.mapUniqids(mapper),
        args.map(_.mapUniqids(mapper)),
        implicitArgs,
        span
      )
    case AST.Let(id, name, ty, value, body, span) =>
      AST.Let(mapper(id), name, ty.map(_.mapUniqids(mapper)), value.mapUniqids(mapper), body.mapUniqids(mapper), span)
    case AST.Ann(expr, ty, span) =>
      AST.Ann(expr.mapUniqids(mapper), ty.mapUniqids(mapper), span)
    case AST.RecordTypeRef(id, name, span) =>
      AST.RecordTypeRef(mapper(id), name, span)
    case AST.RecordCtor(id, name, args, span) =>
      AST.RecordCtor(mapper(id), name, args.map(_.mapUniqids(mapper)), span)
    case AST.FieldAccess(target, field, span) =>
      AST.FieldAccess(target.mapUniqids(mapper), field, span)
    case AST.EnumTypeRef(id, name, span) =>
      AST.EnumTypeRef(mapper(id), name, span)
    case AST.EnumCaseRef(enumId, caseId, enumName, caseName, span) =>
      AST.EnumCaseRef(mapper(enumId), mapper(caseId), enumName, caseName, span)
    case AST.EnumCtor(enumId, caseId, enumName, caseName, args, span) =>
      AST.EnumCtor(mapper(enumId), mapper(caseId), enumName, caseName, args.map(_.mapUniqids(mapper)), span)
    case AST.MetaCell(cell, span) =>
      AST.MetaCell(cell, span)
