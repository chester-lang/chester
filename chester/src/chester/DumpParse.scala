package chester
import chester.syntax._
import chester.error._
import chester.core._
import chester.tyck._

object DumpParse {
  def main(args: Array[String]): Unit = {
    given parseReporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val src = "def main(): Unit = { strings.ToUpper(s) };"
    val source = Source(FileNameAndContent("file", src))
    val chars = CharReader.read(source).toOption.get
    val tokens = Tokenizer.tokenize(chars).toOption.get
    val res = Parser.parseFile(tokens)
    println(res)
  }
}
