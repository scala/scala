class Bob[T]
object Test {
  implicit def foo2bar[T](xs: List[T]): Bob[T] = new Bob[T]
  var x: Bob[Int] = null
  x = List(1,2,3)
  // the problem here was that somehow the type variable that was used to infer the type argument for List.apply
  // would accumulate several conflicting constraints
  // can't reproduce with
}