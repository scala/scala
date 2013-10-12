class A {
  (null: Any) match {
    // We don't want "symbol not found errors" for `a` and `b` in the case body.
    case Goop(a, b, c) => Tuple2(a, b)
  }
}
