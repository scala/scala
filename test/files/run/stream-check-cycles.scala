object Test extends App {
  println(Stream().force)
  println(Stream(1).force)
  println(Stream(1, 2).force)
  println(Stream(1, 2, 3).force)
  println

  val sFinite: Stream[Int] = 1 #:: 2 #:: Stream.empty
  sFinite(1)

  // end not yet reached
  println(sFinite.hasDefiniteSize)
  println(sFinite)
  println

  // end reached
  try sFinite(2) catch { case _: IndexOutOfBoundsException => /* expected */ }
  println(sFinite.hasDefiniteSize)
  println(sFinite)
  println

  val sInfinite: Stream[Int] = 1 #:: 2 #:: 3 #:: sInfinite
  sInfinite(10)

  // always infinite
  println(sInfinite.hasDefiniteSize)
  println(sInfinite)
  println

  lazy val sInf1: Stream[Int] = 4 #:: 5 #:: sInf2
  lazy val sInf2: Stream[Int] = 1 #:: 2 #:: 3 #:: sInf1
  sInf1(10)
  sInf2(10)

  // always infinite
  println(sInf1.hasDefiniteSize)
  println(sInf1)
  println(sInf2.hasDefiniteSize)
  println(sInf2)
  println
}
