
trait T {

  def f = for (implicit a <- Some("17", 42) ; implicit (b, c) = a) yield implicitly[Int]

  def g = for (implicit i <- 1 to 10 if implicitly[Int] < 7 ; implicit s = implicitly[Int].toString) println(implicitly[String])
}
