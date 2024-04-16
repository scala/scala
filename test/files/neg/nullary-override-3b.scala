//> using options -Werror -Wunused:nowarn -Xsource:3
//
// P has parens
class P { def x(): Int = 3 }
// Q is questionable
class Q extends P { override def x: Int = 4 }

trait T1 { def x: String   = "1" }
trait T2 { def x(): String = "2" }

class Mix12a extends T1 with T2 { override def x   = "12a" }
// the rest in nullary-override-3a
