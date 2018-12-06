object Test extends App {
  import collection.mutable.ListBuffer

  def newLB = ListBuffer(sym"a", sym"b", sym"c", sym"d", sym"e")

  def iiobe[A](f: => A) =
    try { f }
    catch { case ex: IndexOutOfBoundsException => println(ex) }

  val lb0 = newLB
  iiobe( lb0.insert(-1, sym"x") )

  val lb1 = newLB
  iiobe( lb1.insertAll(-2, Array(sym"x", sym"y", sym"z")) )

  val lb2 = newLB
  iiobe( lb2.update(-3, sym"u") )

  val lb3 = newLB
  iiobe( lb3.update(-1, sym"u") )
  iiobe( lb3.update(5, sym"u") )
}
