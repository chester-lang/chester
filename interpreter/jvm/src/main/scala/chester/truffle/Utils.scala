package chester.truffle

import chester.error.unreachable
import com.oracle.truffle.api.{CallTarget, TruffleLanguage}
import chester.reader.*
import chester.syntax.core.*
import chester.truffle.ChesterLang.ChesterRootNode
import chester.tyck.*

object Utils {
  @throws[Exception]
  def parse(lang: ChesterLang, request: TruffleLanguage.ParsingRequest): CallTarget =
    ChesterReader
      .parseTopLevel(FileNameAndContent(request.getSource.getPath, request.getSource.getCharacters.toString))
      .fold(
        _err => ???,
        parsedBlock =>
          Tycker.check(parsedBlock) match {
            case TyckResult.Success(result, _, _) =>
              val t: Term = result.wellTyped
              val root = new ChesterRootNode(lang, t)
              root.getCallTarget
            case TyckResult.Failure(_, _, _, _) => ???
            case _                                   => unreachable()
          }
      )
}
