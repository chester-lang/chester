package chester.cli

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, StmtAST, Telescope}
import chester.utils.io.{IO, Runner}
import chester.utils.io.* // brings map/flatMap extensions into scope

/** A minimal evaluator for elaborated Chester ASTs.
  *
  * It supports integers, strings, booleans, tuples/lists, function application, let bindings, and block sequencing. Built-ins implemented:
  *   - `+` on integers
  *   - `println` (uses the provided IO implementation)
  */
object Evaluator:

  sealed trait Value
  case class IntV(value: BigInt) extends Value
  case class StringV(value: String) extends Value
  case class BoolV(value: Boolean) extends Value
  case object UnitV extends Value
  case class TupleV(values: Vector[Value]) extends Value
  case class ListV(values: Vector[Value]) extends Value
  case class Closure(params: Vector[String], body: AST, env: Env) extends Value
  case class Builtin(name: String, arity: Int, run: Vector[Value] => Value) extends Value
  case class BuiltinIO[F[_]](name: String, arity: Int, run: Vector[Value] => F[Value]) extends Value

  case class Env(bindings: Map[String, Value]) {
    def get(name: String): Option[Value] = bindings.get(name)
    def extend(name: String, value: Value): Env = copy(bindings = bindings + (name -> value))
    def extendAll(pairs: Iterable[(String, Value)]): Env = copy(bindings = bindings ++ pairs)
  }

  private def defaultEnv[F[_]](using io: IO[F], runner: Runner[F]): Env = {
    Env(
      Map(
        "true" -> BoolV(true),
        "false" -> BoolV(false),
        "+" -> Builtin(
          "+",
          2,
          {
            case Vector(IntV(a), IntV(b)) => IntV(a + b)
            case other                    => throw new RuntimeException(s"Invalid arguments to '+': $other")
          }
        ),
        "println" -> BuiltinIO(
          "println",
          1,
          {
            case Vector(v) =>
              val msg = valueToString(v)
              io.println(msg).map(_ => UnitV)
            case other =>
              throw new RuntimeException(s"Invalid arguments to println: $other")
          }
        )
      )
    )
  }

  /** Evaluate an AST, returning a value in the provided effect F. */
  def eval[F[_]](ast: AST)(using io: IO[F], runner: Runner[F]): F[Value] =
    evalWithEnv(ast, defaultEnv)

  private def evalWithEnv[F[_]](ast: AST, env: Env)(using io: IO[F], runner: Runner[F]): F[Value] = {
    ast match
      case AST.IntLit(v, _)     => runner.pure(IntV(v))
      case AST.NaturalLit(v, _) => runner.pure(IntV(v))
      case AST.StringLit(s, _)  => runner.pure(StringV(s))
      case AST.Ref(_, name, _) =>
        env.get(name) match
          case Some(v) => runner.pure(v)
          case None    => runner.pure(UnitV) // undefined, keep evaluator total

      case AST.Tuple(elems, _) =>
        sequence(elems.map(e => evalWithEnv(e, env))).map(vals => TupleV(vals.toVector))

      case AST.ListLit(elems, _) =>
        sequence(elems.map(e => evalWithEnv(e, env))).map(vals => ListV(vals.toVector))

      case AST.Block(stmts, tail, _) =>
        evalBlock(stmts, tail, env)

      case AST.Let(_, name, _, value, body, _) =>
        evalWithEnv(value, env).flatMap(v => evalWithEnv(body, env.extend(name, v)))

      case AST.Lam(teles, body, _) =>
        val params = flattenTeleNames(teles)
        runner.pure(Closure(params, body, env))

      case AST.App(func, args, _, _) =>
        for
          fVal <- evalWithEnv(func, env)
          argVals <- sequence(args.map(a => evalWithEnv(a.value, env)))
          res <- applyFunc(fVal, argVals.toVector)
        yield res

      case AST.Ann(expr, _, _) =>
        evalWithEnv(expr, env)

      case AST.FieldAccess(target, field, _) =>
        evalWithEnv(target, env).map { v =>
          v match
            case TupleV(values) if field.matches("^_\\d+$") =>
              val idx = field.drop(1).toInt - 1
              values.lift(idx).getOrElse(UnitV)
            case _ => UnitV
        }

      case _ =>
        runner.pure(UnitV)
  }

  private def evalBlock[F[_]](stmts: Vector[StmtAST], tail: AST, env: Env)(using io: IO[F], runner: Runner[F]): F[Value] = {
    val initial: F[Env] = runner.pure(env)
    val envF = stmts.foldLeft(initial) { (accF, stmt) =>
      accF.flatMap { acc =>
        stmt match
          case StmtAST.Def(_, name, teles, _, body, _) =>
            val params = flattenTeleNames(teles)
            val closure = Closure(params, body, acc)
            runner.pure(acc.extend(name, closure))
          case _ =>
            // Ignore other statement forms for now
            runner.pure(acc)
      }
    }
    envF.flatMap(env2 => evalWithEnv(tail, env2))
  }

  private def flattenTeleNames(teles: Vector[Telescope]): Vector[String] =
    teles.flatMap(_.params.map(_.name))

  private def applyFunc[F[_]](f: Value, args: Vector[Value])(using io: IO[F], runner: Runner[F]): F[Value] = {
    f match
      case Closure(params, body, closureEnv) =>
        val (actualArgs, extendedParams) = {
          if params.nonEmpty && params.lastOption.contains("k") && args.length == params.length - 1 then
            // Effect CPS shape: synthesize a default continuation that just returns its argument.
            val cont = Builtin(
              "k",
              1,
              {
                case Vector(v) => v
                case _         => UnitV
              }
            )
            (args :+ cont, params)
          else (args, params)
        }
        val extended = closureEnv.extendAll(extendedParams.zip(actualArgs))
        evalWithEnv(body, extended)
      case Builtin(_, arity, run) if args.length == arity =>
        runner.pure(run(args))
      case b: BuiltinIO[F] if args.length == b.arity =>
        b.run(args)
      case _ =>
        runner.pure(UnitV)
  }

  private def sequence[F[_], A](fs: Iterable[F[A]])(using runner: Runner[F]): F[List[A]] =
    fs.foldLeft(runner.pure(List.empty[A]))((accF, f) => accF.flatMap(acc => f.map(v => acc :+ v)))

  def valueToString(v: Value): String = v match
    case IntV(n)               => n.toString
    case StringV(s)            => s""""$s""""
    case BoolV(b)              => b.toString
    case UnitV                 => "()"
    case TupleV(xs)            => xs.map(valueToString).mkString("(", ", ", ")")
    case ListV(xs)             => xs.map(valueToString).mkString("[", ", ", "]")
    case Closure(_, _, _)      => "<function>"
    case Builtin(name, _, _)   => s"<builtin $name>"
    case BuiltinIO(name, _, _) => s"<builtin-io $name>"
