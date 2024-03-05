
//> using options -Werror -Wunused:locals -Xlint:missing-interpolator -Wmacros:before

import Macro.id

object Test extends App {
  println {
    id {
      val unusedVariable = "42".toInt
      println("hello, world of $unusedVariable")
    }
  }
}
