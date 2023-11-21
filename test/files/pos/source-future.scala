
//> using options -Werror -Xlint -Xsource:future

import scala.util.*

class C {
  def f[A](body: => A): Try[A] = Try(body)
}
