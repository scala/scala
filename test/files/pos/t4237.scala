class A {
  (new { def field = 0; def field_=(i: Int) = () }).field = 5 // compiles as expected
  (new { def field(implicit i: Int) = 0; def field_=(i: Int) = () }).field = 5 // compiles even with implicit params on getter
  (new { def field = 0; def field_=[T](i: Int) = () }).field = 5 // compiles with type param on setter
  (new { def field[T] = 0; def field_=(i: Int) = () }).field = 5 // DOESN'T COMPILE
}