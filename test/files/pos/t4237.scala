class A {
  (new { def field = 0; def field_=(i: Int) = () }).field = 5 // compiles as expected
  (new { def field(implicit i: Int) = 0; def field_=(i: Int) = () }).field = 5 // compiles even with implicit params on getter
  (new { def field = 0; def field_=[T](i: Int) = () }).field = 5 // compiles with type param on setter
  (new { def field[T] = 0; def field_=(i: Int) = () }).field = 5 // DIDN'T COMPILE

  class Imp
  implicit val imp: Imp = new Imp
  implicit val implicitList: List[Int] = null

  // compiles even with implicit params on setter
  (new { def field(implicit i: Int) = 0; def field_=(i: Int)(implicit j: Imp) = () }).field = 5
  (new { def field(implicit i: Int) = 0; def field_=[T <: Imp](i: Int)(implicit j: T) = () }).field = 5
  // was reassignment to val
  (new { def field[T](implicit ts: List[T]) = 0; def field_=[T](i: Int)(implicit ts: List[T]) = () }).field = 5
  (new { def field[T](implicit ts: List[T]) = 0; def field_=[T](i: T)(implicit ts: List[T]) = () }).field = 5
}
