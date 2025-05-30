package chester.tsmorph
import typings.tsMorph.mod
import chester.i18n.*

/** Tests for interacting with ts-morph using ScalaJS with ScalablyTyped bindings
  */
object TsMorphTest {

  /** Test basic ts-morph functionality
    */
  def testTsMorph(): Unit = {
    println("Testing ts-morph functionality using ScalablyTyped bindings...")

    try {
      // Create a new project using the proper typed bindings
      val project = new mod.Project()

      // Create a source file with some sample TypeScript code
      val sourceFileText = """
        export interface Person {
          id: number;
          name: string;
          email?: string;
        }
        
        export class PersonService {
          private people: Person[] = [];
          
          addPerson(person: Person): void {
            this.people.push(person);
          }
          
          getPerson(id: number): Person | undefined {
            return this.people.find(p => p.id === id);
          }
        }
      """

      // Create the source file
      println("Creating source file...")
      val sourceFile = project.createSourceFile("test.ts", sourceFileText)

      // Print basic information about the source file
      println(t"Source file created: ${sourceFile.getFilePath()}")
      println(t"Syntax kind: ${sourceFile.getKind()}")

      // Get the text of the source file
      println(t"File text: ${sourceFile.getText().take(50)}...")

      // Try to format the source file
      println("\nFormatting source file...")
      sourceFile.formatText()

      // See if the file is in memory or saved
      println(t"Is file saved: ${sourceFile.isSaved()}")

      println("\nts-morph test completed successfully!")
    } catch {
      case e: Throwable =>
        println(t"Error testing ts-morph: ${e.getMessage}")
        e.printStackTrace()
    }
  }
}
