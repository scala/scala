//> using options -Vimplicit-conversions -Xlint -Xsource:3 -Xsource-features:any2stringadd

import scala.Predef.*

case class Money(value: Double)

object Test extends App {
  println {
    Money(3.14) + " is a lotta dough!"
  }
}
