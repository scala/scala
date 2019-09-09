// scalac: -Werror -Xlint

object O { O() ; def f(): Unit = O() }
case class O private (x: Int = 3)

object Q { Q[Int]() ; def f(): Unit = Q[Int]() }
case class Q[A] private (x: Int = 3)
