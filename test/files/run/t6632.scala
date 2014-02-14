object Test extends App {
  import collection.mutable.ListBuffer

  def newLB = ListBuffer('a, 'b, 'c, 'd, 'e)

  def iiobe[A](f: => A) =
    try { f }
    catch { case ex: IndexOutOfBoundsException => println(ex) }

  val lb0 = newLB
  iiobe( lb0.insert(-1, 'x) )

  val lb1 = newLB
  iiobe( lb1.insertAll(-2, Array('x, 'y, 'z)) )

  val lb2 = newLB
  iiobe( lb2.update(-3, 'u) )

  val lb3 = newLB
  iiobe( lb3.updated(-1, 'u) )
  iiobe( lb3.updated(5, 'u) )
}
