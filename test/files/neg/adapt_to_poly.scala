class Test {
  val f: Int => Iterable[Int] = x => List(x)
  Option(1).flatMap[String](f) // don't drop explicitly provided args during adapt
}
