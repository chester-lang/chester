package chester.utils

import fastparse.ParserInput

def encodeString(x: String): String = x
  .replace("\\", "\\\\")
  .replace("\n", "\\n")
  .replace("\t", "\\t")
  .replace("\r", "\\r")
  .replace("\"", "\\\"")
def parserInputToLazyList(pi: ParserInput): LazyList[String] = {
  LazyList
    .from(0)
    .takeWhile(pi.isReachable)
    .map(index => pi.slice(index, index + 1))
}
