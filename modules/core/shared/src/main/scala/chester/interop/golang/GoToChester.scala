package chester.interop.golang

import scala.language.experimental.genericNumberLiterals

import chester.core.{AST, Implicitness, Param, Telescope}
import chester.tyck.GoImportSignature
import chester.uniqid.Uniqid

/** Bridge from Go type information (JSON format) into Chester types.
  *
  * This converts Go package exports (functions, interfaces, structs) into
  * Chester type signatures for type checking.
  */
object GoToChester:

  /** Generate a GoImportSignature from Go source code (using portable Scala parser). */
  def fromGoSource(goSource: String, sourceRef: chester.reader.Source, packagePath: String): GoImportSignature = {
    import chester.syntax.GoDeclParser
    val json = GoDeclParser.parse(goSource, sourceRef)
    val jsonStr = ujson.write(json)
    packageSignature(jsonStr, packagePath)
  }

  /** Generate a GoImportSignature from JSON output (legacy format from go-type-extractor).  */
  def packageSignature(jsonOutput: String, packagePath: String): GoImportSignature = {
    val parsed = ujson.read(jsonOutput)
    val exports = extractExports(parsed)
    GoImportSignature(exports, packagePath)
  }

  private def extractExports(json: ujson.Value): Vector[Param] = {
    val fields = scala.collection.mutable.LinkedHashMap.empty[String, AST]

    def add(name: String, ty: AST): Unit =
      if !fields.contains(name) then fields.update(name, ty)

    // Extract functions
    json.obj.get("functions").foreach { funcsJson =>
      funcsJson.arr.foreach { funcJson =>
        val name = funcJson.obj("name").str
        val funcType = chesterTypeFromFunction(funcJson)
        add(name, funcType)
      }
    }

    // Extract interfaces (as record types with method fields)
    json.obj.get("interfaces").foreach { ifacesJson =>
      ifacesJson.arr.foreach { ifaceJson =>
        val name = ifaceJson.obj("name").str
        val ifaceType = chesterTypeFromInterface(ifaceJson)
        add(name, ifaceType)
      }
    }

    // Extract structs (as record types)
    json.obj.get("structs").foreach { structsJson =>
      structsJson.arr.foreach { structJson =>
        val name = structJson.obj("name").str
        val structType = chesterTypeFromStruct(structJson)
        add(name, structType)
      }
    }

    fields.toVector.map { case (name, ty) =>
      Param(Uniqid.make, name, ty, Implicitness.Explicit, None)
    }
  }

  private def chesterTypeFromFunction(funcJson: ujson.Value): AST = {
    val params = funcJson.obj.get("params").map { paramsJson =>
      paramsJson.arr.zipWithIndex.map { case (paramJson, idx) =>
        val name = paramJson.obj.get("name").map(_.str).getOrElse(s"param$idx")
        val typeName = paramJson.obj("type").str
        val paramType = goTypeToChester(typeName)
        Param(Uniqid.make, name, paramType, Implicitness.Explicit, None)
      }.toVector
    }.getOrElse(Vector.empty)

    val results = funcJson.obj.get("results").map { resultsJson =>
      resultsJson.arr.map { resultJson =>
        val typeName = resultJson.obj("type").str
        goTypeToChester(typeName)
      }.toVector
    }.getOrElse(Vector.empty)

    // Go functions can return multiple values - represent as tuple
    val returnType = results match
      case Vector()     => AST.TupleType(Vector.empty, None) // void
      case Vector(single) => single
      case multiple    => AST.TupleType(multiple, None)

    AST.Pi(Vector(Telescope(params, Implicitness.Explicit)), returnType, Vector.empty, None)
  }

  private def chesterTypeFromInterface(ifaceJson: ujson.Value): AST = {
    // Interfaces in Go are represented as their own type
    // For now, map to AnyType since we're exposing them as runtime values in the signature
    // The methods are captured as Param fields in extractExports
    AST.AnyType(None)
  }

  private def chesterTypeFromMethod(methodJson: ujson.Value): AST = {
    // Methods are like functions
    chesterTypeFromFunction(methodJson)
  }

  private def chesterTypeFromStruct(structJson: ujson.Value): AST = {
    // Structs are represented as their own type
    // For now, map to AnyType since we're exposing them as runtime values in the signature
    AST.AnyType(None)
  }

  /** Map Go type strings to Chester types. */
  private def goTypeToChester(goType: String): AST = {
    goType match
      // Primitives
      case "string" => AST.StringType(None)
      case "int" | "int8" | "int16" | "int32" | "int64" |
           "uint" | "uint8" | "uint16" | "uint32" | "uint64" |
           "byte" | "rune" => AST.IntegerType(None)
      case "float32" | "float64" => AST.AnyType(None) // TODO: Add float type to Chester
      case "bool" => AST.AnyType(None) // TODO: Add bool type to Chester
      case "error" => AST.AnyType(None) // Go's error interface

      // Slices and arrays
      case s if s.startsWith("[]") =>
        val elemType = goTypeToChester(s.drop(2))
        AST.ListType(elemType, None)

      // Pointers (dereference to underlying type for now)
      case s if s.startsWith("*") =>
        goTypeToChester(s.drop(1))

      // Maps
      case s if s.startsWith("map[") =>
        // Extract key and value types
        // For now, represent as Any. TODO: Add proper map type
        AST.AnyType(None)

      // Channels
      case s if s.contains("chan ") =>
        AST.AnyType(None) // TODO: Add channel type

      // Named types from other packages (e.g., "os.File")
      case s if s.contains(".") =>
        // For now, treat as opaque type
        AST.AnyType(None)

      // Everything else
      case _ => AST.AnyType(None)
  }
