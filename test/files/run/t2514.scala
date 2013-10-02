

import scala.language.{ implicitConversions, postfixOps, reflectiveCalls }

object Test
{
  implicit def x[A](a: A) = new { def xx = a }

  def main(args: Array[String]): Unit = {
    val r1 = 12 xx;
    val r2 = 12.xx
    val r3 = 12.`xx`
    val r4 = 12.xx + 12.xx
    val r5 = 12.`xx` + 12.xx
    val r6 = 12.3.`xx` + 12.xx

    assert(r5 == 24)
  }
}
