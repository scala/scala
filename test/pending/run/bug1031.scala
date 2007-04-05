abstract class Tree
case class Foo(xs: List[Int]) extends Tree

object test extends Application {
  Foo(Nil) match {
    case Foo(xs: List[_]) =>
      Console.println(xs)
    case _ =>
      ;
  }
}
