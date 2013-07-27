
import PartialFunction.cond
import util._

object Test extends App {
  val days = (1 to 12).toList

  days map s"On the $_th day of Christmas" foreach println

  val rf = (n: Int) => s"\\*{$_}"(n).r
  def stars(n: Int)(s: String) = {
    val r = rf(n)
    cond(s) { case r(_*) => true }
  }
  Console println stars(5)("*****")

  days zip days map s"$_ by $_".tupled foreach println

  object I { def unapply(x: String): Option[Int] = Try(x.toInt).toOption }
  implicit class RX(val sc: StringContext) {
    object rx {
      def apply(args: Any*) = sc s (args: _*)
      def unapplySeq(s: String): Option[Seq[String]] = sc.parts.mkString("(.+)").r.unapplySeq(s)
    }
  }

  Console println ("2 by 4" match {
    case rx"${I(a)} by ${I(b)}" => a+b
    case _                      => -1
  })
  Console println ("2 by 4" match {
    case rx"${_} by ${I(b)}"    => b    // pattern placeholder
    case _                      => -1
  })
  Console println ("2 by 4" match {
    case rx"$_ by ${I(b)}"      => b    // is emitted this way, too
    case _                      => -1
  })
}
