package chester

object TestFindDefExtension {
  def main(args: Array[String]): Unit = {
    val stdlib = CLI.loadStdlib("go")
    val content = java.nio.file.Files.readString(java.nio.file.Paths.get("test_extension.chester"))
    val fullContent = stdlib + "\n" + content
    
    val source = chester.Source(FileNameAndContent("file.chester", fullContent))
    given r1: VectorReporter[ParseError] = new VectorReporter[ParseError]()
    val chars = chester.CharReader.read(source).toOption.get
    val tokens = chester.Tokenizer.tokenize(chars).toOption.get
    val cst = chester.Parser.parseFile(tokens)
    
    def walk(node: CST): Unit = {
      node match {
        case CST.SeqOf(seqElems, _) =>
          val firstTwo = seqElems.toVector.take(2).map {
            case CST.Symbol(name, _) => name
            case _ => "?"
          }.mkString(" ")
          if (firstTwo == "def extension") {
            println(s"FOUND IT! $node")
          }
          if (firstTwo == "extension def") {
             println(s"FOUND extension def! $node")
          }
          seqElems.toVector.foreach(walk)
        case CST.Block(elems, tail, _) =>
          elems.foreach(walk)
          tail.foreach(walk)
        case CST.Tuple(elems, _) => elems.foreach(walk)
        case CST.ListLiteral(elems, _) => elems.foreach(walk)
        case _ => ()
      }
    }
    
    walk(cst)
  }
}
