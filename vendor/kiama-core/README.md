# README - kiama-core

taken from https://github.com/inkytonik/kiama/tree/3bcc03ae08e60c79b3c4b9bad3ae9ae233979d4a

Copied files from scala-2.13 and scala-2.not11 to scala3 folder

No guava dependencies. Patched without modifying line numbers. For Memoiser, ConcurrentHashMap and synchronizedMap(WeakHashMap) are used. For other places, implementations are removed and placeholder `???` is added

## Workaround for Scala Native

Source.scala trait Source 

`lazy val (lineStarts, charCount, lineCount)` -> `val (lineStarts, charCount, lineCount)`