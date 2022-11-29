// scalac: -Werror -Xsource:3 -Xlint:nullary-override
//
class B extends A { override def x(): Int = 4 }

trait T1 { def x: String   = "1" }
trait T2 { def x(): String = "2" }
class Mix12b extends T1 with T2 { override def x() = "12b" }
