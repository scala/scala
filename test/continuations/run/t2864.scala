import scala.util.continuations._
object Test {

  def double[B](n : Int)(k : Int => B) : B = k(n * 2)

  def main(args : Array[String]) {
     reset {
       val result1 = shift(double[Unit](100))
       val result2 = shift(double[Unit](result1))
       println(result2)
     }
  }

  def foo: Int @cps[Int] = {
    val a0 = shift((k:Int=>Int) => k(0))
    val x0 = 2
    val a1 = shift((k:Int=>Int) => x0)
    0
  }

/*
  def bar: ControlContext[Int,Int,Int] = {
    shiftR((k:Int=>Int) => k(0)).flatMap { a0 =>
    val x0 = 2
    shiftR((k:Int=>Int) => x0).map { a1 =>
    0
    }}
  }
*/
}