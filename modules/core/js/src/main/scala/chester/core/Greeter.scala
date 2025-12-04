package chester.core

/** Scala.js-specific implementation */
object Greeter {
  def greet(name: String): String = 
    s"Hello, $name! Welcome to Chester multiplatform project."
  
  def platform: String = "JavaScript (Scala.js)"
}
