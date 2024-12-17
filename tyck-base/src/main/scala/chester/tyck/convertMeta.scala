package chester.tyck

import chester.syntax.concrete.ExprMeta
import chester.syntax.core.interface.TermMeta

def convertMeta(meta: Option[ExprMeta]): Option[TermMeta] = for {
  m <- meta
  s <- m.sourcePos
} yield TermMeta(s)
