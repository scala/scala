object Test {
  // no fruitless warning as Tuple2 isn't (yet) final.
  // The corresponding `neg` test will treat it as final
  // for the purposes of these tests under -Xfuture.
  (1, 2) match { case Seq() => 0; case _ => 1 }
}
