import language.existentials

object Test {
  class Def[T]
  class Exp[T]
  class Contra[-T] { def head[T1 <: T] : T1 = ??? }
  class Cov[+T] { }

  case class ArrayApply[T](x: Exp[Array[T]], i: Exp[Int], j: Exp[_]) extends Def[T]

  val IntArrayApply = ArrayApply[Int](new Exp[Array[Int]], new Exp[Int], new Exp[Int])

  def f(x: Any) = x match {
    case xs: Iterable[Any]    => xs.head // okay
    case _                    => 0
  }
  def f2(x: Any) = x match {
    case xs: Iterable[String] => xs.head // unchecked
    case _                    => 0
  }
  def f3(x: Any) = x match {
    case xs: Set[Any] => xs.head // unchecked
    case _            => 0
  }
  def f4(x: Any) = x match {
    case xs: Map[Any, Any] => xs.head // unchecked
    case _                 => 0
  }

  def cf1(x: Any) = x match {
    case xs: Contra[Nothing] => xs.head // okay
    case _                   => 0
  }
  def cf2(x: Any) = x match {
    case xs: Contra[List[Nothing]] => xs.head // unchecked
    case _                         => 0
  }

  def co1(x: List[Cov[List[Int]]]) = x match {
    case _: Seq[Cov[Seq[Any]]] => true  // okay
    case _                     => false
  }

  def g[T](x: Def[T]) = x match {
    case ArrayApply(x: Exp[Array[T]], i: Exp[Int], _) => x // okay
    case _                                            => 0
  }

  def g2[T](x: Def[T]) = x match {
    case ArrayApply(x: Exp[Array[T]], _, j: Exp[String]) => x // unchecked
    case _                                               => 0
  }

  def g3[T](x: Any) = x match {
    case ArrayApply(x: Exp[Array[T]], _, _) => x // unchecked
    case _                                  => 0
  }

  def g4 = IntArrayApply match {
    case ArrayApply(x: Exp[Array[Int]], _, _) => x // okay
    case _                                    => ()
  }
  def g5[T](x: ArrayApply[Int]) = x match {
    case ArrayApply(x: Exp[Array[Int]], _, _) => x // okay
    case _                                    => 0
  }

  // Nope
  //
  // def g5 = IntArrayApply match {
  //   case ArrayApply(x: Exp[Array[String]], _, _) => x // nope
  //   case _                                       => ()
  // }
}
