package chester.utils

import chester.core.Greeter

/** Utility functions that depend on core */
object StringUtils {
  def reverse(s: String): String = s.reverse
  
  def greetReversed(name: String): String = 
    Greeter.greet(reverse(name))
  
  def isPalindrome(s: String): Boolean = {
    val normalized = s.toLowerCase.replaceAll("\\s", "")
    normalized == normalized.reverse
  }
}
