package chester.tyck

import scala.language.experimental.genericNumberLiterals

import chester.core.{Implicitness, JSImportKind, Param}
import chester.uniqid.Uniqid

/** Minimal, loss-tolerant representation of a JS/TS module's value-level exports.
  *
  * This is used to type Chester `import` bindings by synthesizing a record type whose fields correspond to exported values.
  */
final case class JSImportSignature(
    fields: Vector[Param],
    kind: JSImportKind = JSImportKind.Namespace
)

object JSImportSignature:
  /** Normalize node builtins so `node:fs` and `fs` share the same type source. */
  def normalizeModuleSpecifier(spec: String): String =
    if spec.startsWith("node:") then spec.stripPrefix("node:") else spec

  /** Create a stable Chester identifier for a generated record type from a module specifier. */
  def recordTypeNameFor(spec: String): String = {
    val normalized = normalizeModuleSpecifier(spec)
    val builder = new java.lang.StringBuilder("JSImport_")
    // Avoid `java.util.stream.*` (not available on Scala.js).
    normalized.foreach { ch =>
      if ch.isLetterOrDigit then builder.append(ch) else builder.append('_')
    }
    builder.toString
  }

  /** Freshen parameter IDs so signatures can be re-used safely across elaborations. */
  def freshenParams(fields: Vector[Param]): Vector[Param] =
    fields.map(p => p.copy(id = Uniqid.make, implicitness = Implicitness.Explicit))
