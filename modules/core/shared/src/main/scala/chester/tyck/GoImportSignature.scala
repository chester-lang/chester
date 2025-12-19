package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.Param
import upickle.default.*

/** Representation of a Go package's exported value-level members.
  *
  * This is used to type Chester `import` bindings from Go packages by synthesizing
  * a record type whose fields correspond to exported functions, types, etc.
  */
final case class GoImportSignature(
    fields: Vector[Param],
    packageName: String
) derives ReadWriter

object GoImportSignature:
  /** Normalize Go package paths so different import styles map to the same type source. */
  def normalizePackagePath(spec: String): String = {
    // Remove "go:" prefix if present
    val cleaned = if spec.startsWith("go:") then spec.drop(3) else spec
    // Normalize standard library paths
    cleaned
  }

  /** Generate a unique type name for a Go package import signature. */
  def recordTypeNameFor(spec: String): String = {
    val normalized = normalizePackagePath(spec)
    val builder = new java.lang.StringBuilder("GoImport_")
    normalized.foreach { ch =>
      if ch.isLetterOrDigit then builder.append(ch) else builder.append('_')
    }
    builder.toString
  }

  /** Freshen parameter IDs so signatures can be re-used safely across elaborations. */
  def freshenParams(fields: Vector[Param]): Vector[Param] =
    import chester.uniqid.Uniqid
    import chester.core.Implicitness
    fields.map(p => p.copy(id = Uniqid.make, implicitness = Implicitness.Explicit))
