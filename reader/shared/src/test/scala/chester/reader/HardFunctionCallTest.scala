package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite
import chester.readerv2.ReaderV2.DEBUG

class HardFunctionCallTest extends FunSuite {
  test("OpSeq And FunctionCall F<nospace>[]") {
    val input = "trait #sealed Vector[n: Nat, T: Type]"
    val expected =
      OpSeq(
        seq = Vector(
          Identifier(
            name = "trait",
            meta = None
          ),
          Keyword(
            key = "sealed",
            telescope = Vector(),
            meta = None
          ),
          FunctionCall(
            function = Identifier(
              name = "Vector",
              meta = None
            ),
            telescope = ListExpr(
              terms = Vector(
                OpSeq(
                  seq = Vector(
                    Identifier(
                      name = "n",
                      meta = None
                    ),
                    Identifier(
                      name = ":",
                      meta = None
                    ),
                    Identifier(
                      name = "Nat",
                      meta = None
                    )
                  ),
                  meta = None
                ),
                OpSeq(
                  seq = Vector(
                    Identifier(
                      name = "T",
                      meta = None
                    ),
                    Identifier(
                      name = ":",
                      meta = None
                    ),
                    Identifier(
                      name = "Type",
                      meta = None
                    )
                  ),
                  meta = None
                )
              ),
              meta = None
            ),
            meta = None
          )
        ),
        meta = None
      )
    DEBUG.withValue(false) {
      parseAndCheckBoth(input, expected)
    }

  }
  test("OpSeq And Not FunctionCall F<space>[]") {
    val input = "trait #sealed Vector [n: Nat, T: Type]"
    val expected =
      OpSeq(
        seq = Vector(
          Identifier(
            name = "trait",
            meta = None
          ),
          Keyword(
            key = "sealed",
            telescope = Vector(),
            meta = None
          ),
          Identifier(
            name = "Vector",
            meta = None
          ),
          ListExpr(
            terms = Vector(
              OpSeq(
                seq = Vector(
                  Identifier(
                    name = "n",
                    meta = None
                  ),
                  Identifier(
                    name = ":",
                    meta = None
                  ),
                  Identifier(
                    name = "Nat",
                    meta = None
                  )
                ),
                meta = None
              ),
              OpSeq(
                seq = Vector(
                  Identifier(
                    name = "T",
                    meta = None
                  ),
                  Identifier(
                    name = ":",
                    meta = None
                  ),
                  Identifier(
                    name = "Type",
                    meta = None
                  )
                ),
                meta = None
              )
            ),
            meta = None
          )
        ),
        meta = None
      )
    DEBUG.withValue(false) {
      parseAndCheckBoth(input, expected)
    }

  }

  test("OpSeq And FunctionCall F<nospace>()") {
    val input = "trait #sealed Vector(n: Nat, T: Type)"
    val expected =
      OpSeq(
        seq = Vector(
          Identifier(
            name = "trait",
            meta = None
          ),
          Keyword(
            key = "sealed",
            telescope = Vector(),
            meta = None
          ),
          FunctionCall(
            function = Identifier(
              name = "Vector",
              meta = None
            ),
            telescope = Tuple(
              terms = Vector(
                OpSeq(
                  seq = Vector(
                    Identifier(
                      name = "n",
                      meta = None
                    ),
                    Identifier(
                      name = ":",
                      meta = None
                    ),
                    Identifier(
                      name = "Nat",
                      meta = None
                    )
                  ),
                  meta = None
                ),
                OpSeq(
                  seq = Vector(
                    Identifier(
                      name = "T",
                      meta = None
                    ),
                    Identifier(
                      name = ":",
                      meta = None
                    ),
                    Identifier(
                      name = "Type",
                      meta = None
                    )
                  ),
                  meta = None
                )
              ),
              meta = None
            ),
            meta = None
          )
        ),
        meta = None
      )
    DEBUG.withValue(false) {
      parseAndCheckBoth(input, expected)
    }
  }

  test("OpSeq And Not FunctionCall F<space>()") {
    val input = "trait #sealed Vector (n: Nat, T: Type)"
    val expected =
      OpSeq(
        seq = Vector(
          Identifier(
            name = "trait",
            meta = None
          ),
          Keyword(
            key = "sealed",
            telescope = Vector(),
            meta = None
          ),
          Identifier(
            name = "Vector",
            meta = None
          ),
          Tuple(
            terms = Vector(
              OpSeq(
                seq = Vector(
                  Identifier(
                    name = "n",
                    meta = None
                  ),
                  Identifier(
                    name = ":",
                    meta = None
                  ),
                  Identifier(
                    name = "Nat",
                    meta = None
                  )
                ),
                meta = None
              ),
              OpSeq(
                seq = Vector(
                  Identifier(
                    name = "T",
                    meta = None
                  ),
                  Identifier(
                    name = ":",
                    meta = None
                  ),
                  Identifier(
                    name = "Type",
                    meta = None
                  )
                ),
                meta = None
              )
            ),
            meta = None
          )
        ),
        meta = None
      )
    DEBUG.withValue(false) {
      parseAndCheckBoth(input, expected)
    }
  }
}
