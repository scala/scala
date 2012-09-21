object C {

  def main(args: Array[String]) = {
    val x = new X(new XX(3))
    println(x.i.x + 9)
  }

}

class X[T](val i: XX[T]) extends AnyVal
class XX[T](val x: T) extends AnyVal

object C1 {
  def main(args: Array[String]) {
    val x = new X1(new XX1(Some(3)))
    println(x.i.x.get + 9)
  }
}

class X1[T](val i: XX1[T]) extends AnyVal
class XX1[T](val x: Option[T]) extends AnyVal
