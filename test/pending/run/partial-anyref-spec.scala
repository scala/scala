class Fn[@specialized(Int, AnyRef) -T, @specialized(Int, AnyRef) +R] {
  override def toString = getClass.getName
}

class Fn3[
  @specialized(Int, AnyRef) -T1,
  @specialized(Double, AnyRef) -T2,
  @specialized(Float) -T3,
  @specialized(Byte, AnyRef) +R
] {
  override def toString = getClass.getName
}

object Test {
  def main(args: Array[String]): Unit = {
    println(new Fn[Int, Int])
    println(new Fn[Int, Byte])
    println(new Fn[Int, AnyRef])
    println(new Fn[Byte, Int])
    println(new Fn[Byte, Byte])
    println(new Fn[Byte, AnyRef])
    println(new Fn[AnyRef, Int])
    println(new Fn[AnyRef, Byte])
    println(new Fn[AnyRef, AnyRef])

    println(new Fn3[Int, Int, Int, Int])
    println(new Fn3[Int, Double, Float, Int])
    println(new Fn3[Int, Double, Float, Byte])
    println(new Fn3[AnyRef, Double, AnyRef, Int])
  }
}
