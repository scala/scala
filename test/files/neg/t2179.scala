object Test {
  (Nil:List[List[Double]]).reduceLeft((_: Any, _: Any) => Nil.indices.map(_ => 0d))
}
