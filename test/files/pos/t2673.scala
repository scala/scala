object Test {
  val xs = Array(1, 2, 3)
  (xs, xs).zipped map (_ + _)
}