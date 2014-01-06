case class B[T](b: T, a: List[Int]) // need two args, B must be polymorphic

class A {
  var s: B[Int] = _ // has to be a var

  s.copy(b = foo)
}
