object Test {
  // Treat TupleN as final under -Xfuture for the for the purposes
  // of the "fruitless type test" warning.
  (1, 2) match { case Seq() => 0; case _ => 1 }
}
