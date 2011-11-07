class P {
  def a[@specialized(Int) T](t: T): List[T] = List(t)
}
class FX extends P {
  override def a[@specialized(Int) T](t: T): List[T] = Nil
}
class FX1 extends P {
  override def a[@specialized(Double) T](t: T): List[T] = Nil
}

class FX2 extends P {
  override def a[T](t: T): List[T] = Nil
}

object Test extends App {
  val fx = new FX
  val p = new P
 
  println(fx.a(3))
  println((fx: P).a(3))
  println((fx: P).a(3.0))


  // val d = new Derived[Int]
  // println((d: Base[Int]).m(10))
}
