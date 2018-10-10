object Test {
  // TupleN is final now, so we get a
  // "fruitless type test" warning.
  (1, 2) match { case Seq() => 0; case _ => 1 }
}
