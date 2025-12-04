package chester.i18n

import scala.quoted.*
import scala.language.implicitConversions

trait T {
  def t(args: Any*): String
}

private def tMacro(sc: Expr[StringContext])(using Quotes): Expr[T] = {
  if (false) {
    println(sc.show)
    println(System.getProperty("user.dir"))
    println("aaa")
    // it works
    // Files.write(Paths.get("/Users/.../test.output"), sc.show.getBytes, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
  }
  '{
    new T {
      def t(args: Any*): String =
        $sc.s(args*)
    }
  }
}
implicit inline def t(inline sc: StringContext): T = ${ tMacro('sc) }
