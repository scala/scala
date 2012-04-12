class Range(val from: Int, val to: Int) extends RangeDefault {
  override def foreach(f: Int => Unit): Unit = macro Impls.foreach
}

object Test extends App {
  new Range(1, 10) foreach println
}