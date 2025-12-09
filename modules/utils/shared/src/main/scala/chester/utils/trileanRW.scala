package chester.utils

import spire.math.Trilean
import upickle.default.*
private enum Three derives ReadWriter {
  case True, False, Unknown
}

private def threeToTrilean(three: Three): Trilean = three match {
  case Three.True    => Trilean.True
  case Three.False   => Trilean.False
  case Three.Unknown => Trilean.Unknown
}

private def trileanToThree(trilean: Trilean): Three = trilean match {
  case Trilean.True    => Three.True
  case Trilean.False   => Three.False
  case Trilean.Unknown => Three.Unknown
}

given trileanRW: ReadWriter[Trilean] = {
  readwriter[Three].bimap(trileanToThree, threeToTrilean)
}
