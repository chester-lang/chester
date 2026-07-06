package chester

object TestOffset {
  def main(args: Array[String]): Unit = {
    val stdlib = CompilerPipeline.loadStdlib("go")
    val content = java.nio.file.Files.readString(java.nio.file.Paths.get("test_extension.chester"))
    val fullContent = stdlib + "\n" + content
    
    println(fullContent.substring(2301, 2301+30))
  }
}
