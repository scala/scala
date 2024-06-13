//> using options -Vimplicit-conversions -Xlint -Xsource:3 -Xsource-features:any2stringadd

import scala.Predef.*

case class Money(value: Double)
object Money {
  implicit class MoneySyntax(private val self: Money)  extends AnyVal {
    def +(other: Money): Money = Money(self.value + other.value)
  }
}

object Test extends App {
  println {
    Money(3.14) + Money(1.7)
  }
}
