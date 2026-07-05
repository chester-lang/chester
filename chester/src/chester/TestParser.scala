package chester

object TestParser {
  def main(args: Array[String]): Unit = {
    val content = "extension def test_list_map[A, B](list: List A, f: (a: A) -> B): List B = prim__list_map[A, B](list, f);"
    val source = chester.Source(FileNameAndContent("test.chester", content))
    given r1: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val chars = chester.CharReader.read(source).toOption.get
    val tokens = chester.Tokenizer.tokenize(chars).toOption.get
    val cst = chester.Parser.parseFile(tokens)
    println(cst)
  }
}
