
//> using options -Werror -Xsource:3

import JetBrains.*

class C {
  def f(jb: JetBrains): Int =
    jb match {
      case APPLE  => 42
    }
}
