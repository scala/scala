object Test {
  val xs: List[(Int, Int)] = List((2, 2), null)
  
  def expectMatchError[T](msg: String)(body: => T) {
    try { body ; assert(false, "Should not succeed.") }
    catch { case _: MatchError => println(msg) }
  }

  def main(args: Array[String]): Unit = {
    expectMatchError("I hereby refute null!")( for ((x, y) <- xs) yield x )
    expectMatchError("I denounce null as unListLike!")( (null: Any) match { case List(_*) => true } )
  }
}
