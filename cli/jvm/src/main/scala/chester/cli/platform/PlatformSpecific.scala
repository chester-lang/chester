package chester.cli.platform

import chester.cli.Main.CliConfig
import chester.tyck.SemanticDBGenerator
import chester.utils.*
import chester.i18n.*

def genSemanticDB(config: CliConfig): Unit = {
  val inputPath = config.input.getOrElse {
    println(t"Error: Input path is required.")
    return
  }

  val path = os2.path(inputPath)
  if (!os.exists(path)) {
    println(t"Error: Input path does not exist: $inputPath")
    return
  }
  if (path.ext != "chester") {
    println(t"Error: Input path must be a .chester file: $inputPath")
    return
  }

  // Create a new SemanticDBGenerator instance
  val generator = new SemanticDBGenerator()

  // Process the input path
  generator.processPath(path)

  // Save the SemanticDB file
  val outputPath = path / os.up / (path.baseName + ".semanticdb")
  generator.saveSemanticDB(path.toString, outputPath.toString)

  println(t"SemanticDB generated at: $outputPath")
}

def testFunctionalities(): Unit = {
  println("functionalities test start")
  chester.scala.Test.callit()
  println("functionalities test end")
}
