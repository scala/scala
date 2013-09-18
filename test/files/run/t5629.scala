


import scala.{specialized => spec}



trait GrandParent[@spec(Int) -A] {
  def foo(a: A): Unit
  def bar[B <: A](b: B): Unit = println("grandparent got: %s" format b)
}


trait Parent[@spec(Int) -A] extends GrandParent[A] {
  def foo(a: A) = bar(a)
}


class IntChild extends Parent[Int] {
  override def bar[B <: Int](b: B): Unit = println("int child got: %s" format b)
}


class AnyChild extends Parent[Any] {
  override def bar[B <: Any](b: B): Unit = println("any child got: %s" format b)
}


object Test {

  def main(args: Array[String]) {
    new IntChild().foo(33)
    new AnyChild().foo(33)
  }

}
