object Test {
  sealed abstract class Foo[T]
  case object Bar1 extends Foo[Int]
  case object Bar2 extends Foo[String]
  case object Bar3 extends Foo[Any]
  
  def ex1[T](xs: List[T]) = xs match {
    case ys: List[_]  => "ok"
  }
  def ex2[T](xx: (Foo[T], Foo[T])) = xx match {
    case (Bar1, Bar1) => ()
    case (_, Bar1) => ()
    case (_, Bar3) => ()
    case (_, Bar2) => ()
  }
  def ex3[T](xx: (Foo[T], Foo[T])) = xx match {
    case (_: Foo[_], _: Foo[_]) => ()
  }
  
  def fail1[T](xs: List[T]) = xs match {
    case Nil            => "ok"
    case x :: y :: Nil  => "ok"
  }
  def fail2[T](xs: List[T]) = xs match {
    case _ :: _ => "ok"
  }
  def fail3[T](x: Foo[T]) = x match {
    case Bar1   => "ok"
    case Bar2   => "ok"
  }
  def fail4[T <: AnyRef](xx: (Foo[T], Foo[T])) = xx match {
    case (Bar1, Bar1) => ()
    case (Bar2, Bar3) => ()
    case (Bar3, _) => ()
  }
  def fail5[T](xx: (Foo[T], Foo[T])) = xx match {
    case (Bar1, Bar1) => ()
    case (Bar2, Bar3) => ()
    case (Bar3, _) => ()
  }
  
  def main(args: Array[String]): Unit = {
    
  }
}
