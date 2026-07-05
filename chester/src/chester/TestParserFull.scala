package chester

object TestParserFull {
  def main(args: Array[String]): Unit = {
    val stdlib = CLI.loadStdlib("go")
    val content = java.nio.file.Files.readString(java.nio.file.Paths.get("test_extension.chester"))
    val fullContent = stdlib + "\n" + content
    
    val source = chester.Source(FileNameAndContent("file.chester", fullContent))
    given r1: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val chars = chester.CharReader.read(source).toOption.get
    val tokens = chester.Tokenizer.tokenize(chars).toOption.get
    val cst = chester.Parser.parseFile(tokens)
    
    cst match {
      case CST.Block(elements, _, _) =>
        elements.foreach { e =>
          e match {
            case CST.SeqOf(seqElems, _) =>
              val firstThree = seqElems.toVector.take(3).map {
                case CST.Symbol(name, _) => name
                case _ => "?"
              }.mkString(" ")
              println(s"Elem: $firstThree")
            case _ => println(s"Other: $e")
          }
        }
    }
  }
}
