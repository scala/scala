//> using options -Xsource:2.13
//

case class C(implicit val c: Int)

case class D(implicit c: Int)(s: String)

case class *(implicit c: Int)

case class
import collection._
