package chester.utils

import spire.math.Natural
import upickle.default.*

// Provide a ReadWriter for spire.math.Natural by converting to/from BigInt
given naturalRW: ReadWriter[Natural] =
  readwriter[BigInt].bimap(_.toBigInt, Nat(_))
