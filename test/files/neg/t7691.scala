
object X { val _ = 42 }

object Y { val (_, _) = (42, 17) }

object Z { val _ = 42 ; val _ = 17 }             // was error

object V { val _, _, _ = println("hi") }         // was error

object W { val _: Int = 42 ; val _: Int = 17 }   // was error

object Q { val _ @ _ = 42 ; val _ = 17 }         // was error


object Test {
  def x = X.`_`       // value _ is not a member of object X

  def y = Y.`_`       // still not

  def z = Z.`_`       // still not

  def v = V.`_`       // still not

  def w = W.`_`       // still not

  def q = Q.`_`       // still not

  def h = locally {
    val _ = 42
    val _ = 17        // was error
  }
}
