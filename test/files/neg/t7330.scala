class Y[T]
class Test {
  // TypeTree is not a valid tree for a pattern
  0 match { case Y[_] => }
}