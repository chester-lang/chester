package chester.cli

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Telescope, Implicitness, Param, EffectRef, JSImportKind}
import chester.tyck.{GoImportSignature, JSImportSignature}
import upickle.default.*

import java.nio.file.{Files, Paths}
import scala.sys.process.*
import scala.util.{Failure, Success, Try}

/** Go type extractor that shells out to `go` toolchain to extract package type information */
object GoTypeExtractor {

  /** Extract type signatures from Go packages and return as a map for GoImportSignature */
  def extractPackages(packagePaths: Vector[String]): Map[String, JSImportSignature] = {
    packagePaths.flatMap { pkgPath =>
      extractPackage(pkgPath) match {
        case Success(sig) =>
          val normalizedPath = GoImportSignature.normalizePackagePath(pkgPath)
          Some(normalizedPath -> sig)
        case Failure(err) =>
          System.err.println(s"Warning: Failed to extract package $pkgPath: ${err.getMessage}")
          None
      }
    }.toMap
  }

  // Cache to store detailed function signatures for JSON output
  private val signatureCache = scala.collection.mutable.Map[String, Map[String, FunctionSignature]]()

  /** Extract a single Go package using go doc */
  private def extractPackage(pkgPath: String): Try[JSImportSignature] = Try {
    // Use `go doc -all <package>` to get all exported symbols
    val docOutput = s"go doc -all $pkgPath".!!

    // Parse the output
    val functions = parseGoDoc(pkgPath, docOutput)

    // Store in cache for later JSON generation
    signatureCache(pkgPath) = functions

    // Convert to JSImportSignature format (reusing JS import infrastructure)
    val params = functions.map { case (name, funcSig) =>
      Param(
        id = chester.uniqid.Uniqid.make[AST],
        name = name,
        ty = funcSig.toChesterAST(),
        implicitness = Implicitness.Explicit,
        default = None
      )
    }.toVector

    JSImportSignature(params, JSImportKind.Namespace)
  }

  /** Parse go doc output to extract function signatures */
  private def parseGoDoc(pkgPath: String, docOutput: String): Map[String, FunctionSignature] = {
    val lines = docOutput.split("\n")
    val functions = scala.collection.mutable.Map[String, FunctionSignature]()

    // Pattern: func FunctionName(params) returnType
    val funcPattern = """func\s+([A-Z]\w+)\s*\((.*?)\)\s*(.*)""".r
    // Pattern: func (recv) Method(params) returnType
    val methodPattern = """func\s+\([^)]+\)\s+([A-Z]\w+)\s*\((.*?)\)\s*(.*)""".r

    for (line <- lines) {
      val trimmed = line.trim

      funcPattern.findFirstMatchIn(trimmed) match {
        case Some(m) =>
          val name = Option(m.group(1)).getOrElse("")
          val params = Option(m.group(2)).getOrElse("")
          val returnType = Option(m.group(3)).getOrElse("")

          if name.nonEmpty then
            val sig = parseFunctionSignature(pkgPath, params, returnType)
            functions(name) = sig

        case None =>
          // Try method pattern
          methodPattern.findFirstMatchIn(trimmed) match {
            case Some(m) =>
              val name = Option(m.group(1)).getOrElse("")
              val params = Option(m.group(2)).getOrElse("")
              val returnType = Option(m.group(3)).getOrElse("")

              if name.nonEmpty then
                val sig = parseFunctionSignature(pkgPath, params, returnType)
                functions(name) = sig

            case None => // Not a function line
          }
      }
    }

    functions.toMap
  }

  /** Parse function signature from go doc output */
  private def parseFunctionSignature(pkgPath: String, paramsStr: String, returnStr: String): FunctionSignature = {
    val params = parseParams(paramsStr)
    val returnType = parseReturnType(returnStr)
    val effects = detectEffects(pkgPath)

    FunctionSignature(params, returnType, effects)
  }

  /** Parse parameter list with improved variadic handling */
  private def parseParams(paramsStr: String): Vector[ParamInfo] = {
    if (paramsStr.trim.isEmpty) return Vector.empty

    val params = scala.collection.mutable.ArrayBuffer[ParamInfo]()
    val parts = paramsStr.split(",").map(_.trim)

    for (part <- parts) {
      // Check for variadic: "a ...any" or "...any"
      val variadicPattern = """(\w+)?\s*\.\.\.(\w+)""".r

      variadicPattern.findFirstMatchIn(part) match {
        case Some(m) =>
          // Variadic parameter
          val name = Option(m.group(1)).filter(_.nonEmpty).getOrElse("args")
          val typeStr = Option(m.group(2)).getOrElse("")
          val chesterType = mapGoTypeString(typeStr)
          params += ParamInfo(name, ChesterType.Variadic(chesterType))

        case None =>
          // Regular parameter: "name type" or just "type"
          val tokens = part.split("\\s+").filter(_.nonEmpty)
          if (tokens.nonEmpty) {
            val (name, typeStr) = if (tokens.length >= 2) {
              (tokens.init.mkString(" "), tokens.last)
            } else {
              ("arg", tokens.head)
            }
            val chesterType = mapGoTypeString(typeStr)
            params += ParamInfo(name, chesterType)
          }
      }
    }

    params.toVector
  }

  /** Parse return type */
  private def parseReturnType(returnStr: String): ChesterType = {
    val trimmed = returnStr.trim

    if (trimmed.isEmpty) {
      return ChesterType.Unit
    }

    // Handle multiple return values: (type1, type2)
    if (trimmed.startsWith("(") && trimmed.endsWith(")")) {
      val inner = trimmed.substring(1, trimmed.length - 1)
      val types = inner.split(",").map(t => mapGoTypeString(t.trim))
      return ChesterType.Tuple(types.toVector)
    }

    mapGoTypeString(trimmed)
  }

  /** Map Go type string to Chester type */
  private def mapGoTypeString(goType: String): ChesterType = {
    val normalized = goType.trim

    normalized match {
      case "string"                                                                                         => ChesterType.String
      case "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16" | "uint32" | "uint64" => ChesterType.Int
      case "float32" | "float64"                                                                            => ChesterType.Float
      case "bool"                                                                                           => ChesterType.Bool
      case "error"                                                                                          => ChesterType.Any // Error interface
      case t if t.startsWith("[]") =>
        val elemType = mapGoTypeString(t.drop(2))
        ChesterType.Array(elemType)
      case t if t.startsWith("*") =>
        // Pointer - just unwrap
        mapGoTypeString(t.drop(1))
      case t if t.startsWith("map[") =>
        ChesterType.Any // Maps -> Any for now
      case "interface{}" | "any" => ChesterType.Any
      case _                     => ChesterType.Any // Unknown types
    }
  }

  /** Detect effects based on package */
  private def detectEffects(pkgPath: String): Vector[String] = {
    val ioPkgs = Set("fmt", "os", "io", "io/ioutil", "net", "net/http", "bufio")
    val purePkgs = Set("math", "strings", "strconv", "unicode")

    if (ioPkgs.contains(pkgPath)) {
      Vector("io")
    } else if (purePkgs.contains(pkgPath)) {
      Vector.empty
    } else {
      // Default: assume I/O for unknown packages
      Vector("io")
    }
  }

  /** Chester type representation */
  sealed trait ChesterType {
    def toChesterAST(span: Option[chester.error.Span] = None): AST = this match {
      case ChesterType.String      => AST.StringType(span)
      case ChesterType.Int         => AST.IntegerType(span)
      case ChesterType.Float       => AST.AnyType(span) // No FloatType exists, use Any
      case ChesterType.Bool        => AST.AnyType(span) // No BoolType exists, use Any
      case ChesterType.Any         => AST.AnyType(span)
      case ChesterType.Unit        => AST.TupleType(Vector.empty, span)
      case ChesterType.Array(elem) =>
        // Array T -> List T approximation
        AST.AnyType(span) // TODO: proper list type
      case ChesterType.Tuple(elems) =>
        AST.TupleType(elems.map(_.toChesterAST(span)), span)
      case ChesterType.Variadic(elem) =>
        // Variadic ...T -> we use Any for now since Chester's variadics need special handling
        AST.AnyType(span)
      case ChesterType.Function(params, ret, effects) =>
        // Create Pi type
        val paramAst = params.map { p =>
          Param(
            id = chester.uniqid.Uniqid.make[AST],
            name = p.name,
            ty = p.ty.toChesterAST(span),
            implicitness = Implicitness.Explicit,
            default = None
          )
        }
        val telescope = Telescope(paramAst, Implicitness.Explicit)
        val effectRefs = effects.map(e => EffectRef.User(chester.uniqid.Uniqid.make, e))
        AST.Pi(Vector(telescope), ret.toChesterAST(span), effectRefs, span)
    }
  }

  object ChesterType {
    case object String extends ChesterType
    case object Int extends ChesterType
    case object Float extends ChesterType
    case object Bool extends ChesterType
    case object Any extends ChesterType
    case object Unit extends ChesterType
    case class Array(elem: ChesterType) extends ChesterType
    case class Tuple(elems: Vector[ChesterType]) extends ChesterType
    case class Variadic(elem: ChesterType) extends ChesterType
    case class Function(params: Vector[ParamInfo], ret: ChesterType, effects: Vector[String]) extends ChesterType
  }

  case class ParamInfo(name: String, ty: ChesterType)

  case class FunctionSignature(params: Vector[ParamInfo], returnType: ChesterType, effects: Vector[String]) {
    def toChesterAST(span: Option[chester.error.Span] = None): AST = {
      val paramAst = params.map { p =>
        Param(
          id = chester.uniqid.Uniqid.make[AST],
          name = p.name,
          ty = p.ty.toChesterAST(span),
          implicitness = Implicitness.Explicit,
          default = None
        )
      }
      val telescope = Telescope(paramAst, Implicitness.Explicit)
      val effectRefs = effects.map(e => EffectRef.User(chester.uniqid.Uniqid.make, e))
      AST.Pi(Vector(telescope), returnType.toChesterAST(span), effectRefs, span)
    }
  }

  /** Save extracted signatures to JSON file with detailed parameter information */
  def saveToJson(signatures: Map[String, JSImportSignature], outputPath: String): Unit = {
    val packagesJson = signatures
      .map { case (pkgPath, sig) =>
        // Get cached detailed signatures if available
        val detailedSigs = signatureCache.getOrElse(pkgPath, Map.empty)

        val funcsJson = sig.fields
          .map { param =>
            detailedSigs.get(param.name) match {
              case Some(funcSig) =>
                // Output detailed signature with parameters and return type
                val paramsJson = if (funcSig.params.nonEmpty) {
                  val paramsList = funcSig.params
                    .map { p =>
                      val typeStr = chesterTypeToString(p.ty)
                      s"""        { "name": "${p.name}", "type": "$typeStr" }"""
                    }
                    .mkString(",\n")
                  s"""\n$paramsList\n        """
                } else {
                  ""
                }

                val returnTypeStr = chesterTypeToString(funcSig.returnType)
                val effectsJson = if (funcSig.effects.nonEmpty) {
                  s""",
        "effects": [${funcSig.effects.map(e => s""""$e"""").mkString(", ")}]"""
                } else {
                  ""
                }

                s"""      "${param.name}": {
        "params": [$paramsJson],
        "returns": "$returnTypeStr"$effectsJson
      }"""
              case None =>
                // Fallback to simple format if detailed signature not available
                s"""      "${param.name}": { "type": "Function" }"""
            }
          }
          .mkString(",\n")

        s"""    "$pkgPath": {
      "functions": {
$funcsJson
      }
    }"""
      }
      .mkString(",\n")

    val json = s"""{
  "packages": {
$packagesJson
  }
}"""

    Files.writeString(Paths.get(outputPath), json)
  }

  /** Convert ChesterType to JSON-friendly string representation */
  private def chesterTypeToString(ty: ChesterType): String = ty match {
    case ChesterType.String      => "string"
    case ChesterType.Int         => "int"
    case ChesterType.Float       => "float"
    case ChesterType.Bool        => "bool"
    case ChesterType.Any         => "any"
    case ChesterType.Unit        => "unit"
    case ChesterType.Array(elem) => s"[]${chesterTypeToString(elem)}"
    case ChesterType.Tuple(elems) =>
      s"(${elems.map(chesterTypeToString).mkString(", ")})"
    case ChesterType.Variadic(elem)           => s"...${chesterTypeToString(elem)}"
    case ChesterType.Function(params, ret, _) => "function"
  }
}
