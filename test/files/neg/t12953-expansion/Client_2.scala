
//> using options -Werror -Wunused:locals -Xlint:missing-interpolator -Wmacros:after

import Macro.id

object Test extends App {
  println {
    id {
      println("hello, world of $unusedVariable")
    }
  }
}
