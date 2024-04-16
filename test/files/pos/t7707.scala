//> using options -Werror -Xlint

// uses apply default, ctor default is unused
object O { O() ; def f(): Unit = O() }
case class O private (x: Int = 3)

object Q { Q[Int]() ; def f(): Unit = Q[Int]() }
case class Q[A] private (x: Int = 3)

// normal usage
object P { new P() ; def f(): Unit = new P() }
class P private (x: Int = 3)
