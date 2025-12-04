package chester.utils

class StringUtilsTest extends munit.FunSuite {
  test("reverse should reverse a string") {
    assertEquals(StringUtils.reverse("hello"), "olleh")
  }

  test("isPalindrome should detect palindromes") {
    assert(StringUtils.isPalindrome("racecar"))
    assert(StringUtils.isPalindrome("A man a plan a canal Panama"))
    assert(!StringUtils.isPalindrome("hello"))
  }

  test("greetReversed should use core module") {
    val result = StringUtils.greetReversed("World")
    assert(result.contains("dlroW"))
  }
}
