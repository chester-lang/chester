import chester._
import chester.syntax._
import java.nio.file.{Files, Paths}

@main def runPrintCst() = {
  val content = Files.readString(Paths.get("test_effects.chester"))
  given reporter: VectorReporter[ParseError] = new VectorReporter[ParseError]()
  val tokens = Tokenizer.tokenize(CharReader.read(scala.io.Source.fromString(content)).toSeq).toSeq
  val cst = Parser.parseFile(tokens)
  println(cst)
}
