// #2629, #2639, #2669
object Test2669 {

  def test[T](l: java.util.ArrayList[_ <: T]) = 1
  test(new java.util.ArrayList[String]())

}

import java.util.ArrayList

object Test2629 {
  def main(args: Array[String]): Unit = {
    val l = new ArrayList[String](1)
    val m = new ArrayList(l)

    println(l.size)
    println(m.size)
  }
}


import java.util.Vector

// scalac cannot detect lack of type params, but then throws AssertionError later:
class TVector2639 {
  val b = new Vector  // this line passed without error detected 
  val a = new Vector(1) // this line caused throwing AssertionError when scalac
}
