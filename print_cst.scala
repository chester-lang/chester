import chester.syntax.Parser
import chester.syntax.Tokenizer
import chester.syntax.CharReader
import chester.core.Source
import chester.core.FileNameAndContent
import chester.error.VectorReporter
import chester.syntax.ParseError

object PrintCST {
  def main(args: Array[String]): Unit = {
    given reporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val source = Source(FileNameAndContent("test", "a.b(c)"))
    val chars = CharReader.read(source).toOption.get
    val tokens = Tokenizer.tokenize(chars).toOption.get
    val cst = Parser.parseFile(tokens)
    println(cst)
  }
}
