// scalac: -Ywarn-unused:_ -Xfatal-warnings

import scala.annotation.unused


// this test case is that "foo" is used by subclasses, but not in the default implementation
class Bippy {
  def bip(foo: String): Int = 0           // warn
}

class Bippy2 {
  def bip(@unused foo: String): Int = 0   // no warn
}


// this test case is that "sc" is used by the macros defined within StringContextOps
final class StringContextOps(sc: StringContext)               // warn
final class StringContextOps2(@unused sc: StringContext)      // no warn
