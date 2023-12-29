
//> abusing options -Vprint:parser -Vpos -Yprint-trees:format

object O { val a = 42 }

import O.{a => abcdefghij, a => abcdefghij}
