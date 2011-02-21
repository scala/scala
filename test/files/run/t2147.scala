object Test extends App {
  val s: Seq[Int] = Stream.from(1)
  val res0 = s.map(a => 2).head
  val res1 = Stream.from(1).flatMap(a => List(1)).head

  println((for{a <- Stream.from(1); b <- 1 to 5; if a > 10} yield a).head)
  println((for{a <- Stream.from(1); b <- 1 to a; if a > 10} yield a).head)
}
