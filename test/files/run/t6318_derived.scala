import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def test[T: ClassTag](x: T) {
    println(classTag[T].runtimeClass.isAssignableFrom(x.getClass))
    println(classTag[T].unapply(x))
  }

  class X(val x: Int) extends AnyVal { override def toString = "X" }
  val x = new X(1)
  // the commented line crashes because of SI-6326
  //println(classTag[X].runtimeClass.isAssignableFrom(x.getClass))
  println(classTag[X].unapply(x))
  test(x)
}