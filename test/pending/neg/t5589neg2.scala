class A {
  def f1(x: List[((((Int, (Double, (Float, String))), List[String]), List[Int]), List[Float])]) = {
    for (((((a, (b, (c, d))), es), fs), gs) <- x) yield (d :: es).mkString(", ") // ok
  }

  def f2(x: List[((((Int, (Double, (Float, String))), List[String]), List[Int]), List[Float])]) = {
    for (((((a, (b, (c, (d1, d2)))), es), fs), gs) <- x) yield (d :: es).mkString(", ") // not ok
  }

  def f3(x: List[((((Int, (Double, (Float, String))), List[String]), List[Int]), List[Float])]) = {
    for (((((a, (b, _)), es), fs), gs) <- x) yield (es ::: fs).mkString(", ") // ok
  }
}