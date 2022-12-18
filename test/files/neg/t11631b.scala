
// scalac: -Xsource:3 -Werror -Wunused
// debug: -Xsource:3 -Werror -Wunused -Vdebug -Vlog:_ -Vtyper

import annotation.*

object D {
  //@unused
  private object X {
    private final val x = 42
  }
  class X {
    def f = X.x
  }
  private object Y {
    private final val y = 42
  }
  class Y {
    import Y._
    def f = y
  }
}
