package scala.collection.parallel.benchmarks.arrays




case class Dummy(in: Int) {
  def op = {}
}

object Dummy {
  def dummyOp(a: Int) = { if (a < 0) -1 }
  def dummyOp(d: Dummy) = { if (d.in < 0) d.op }
}









