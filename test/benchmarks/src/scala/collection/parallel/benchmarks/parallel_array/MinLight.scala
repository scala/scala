package scala.collection.parallel.benchmarks.parallel_array





/** Tests reduce method using an operator creating an object as a result. */
class MinLight(sz: Int, p: Int, what: String)
extends Resettable[Int](sz, p, what, (i: Int) => i,  new Array[Any](_), classOf[Int]) {
  def companion = MinLight
  override def repetitionsPerRun = 400
  
  def runpar = pa.min(Ordering[Int])
  def runseq = sequentialMin(sz)
  override def comparisonMap = collection.Map()
}

object MinLight extends Companion {
  def benchName = "min-light";
  def apply(sz: Int, p: Int, what: String) = new MinLight(sz, p, what)
  override def comparisons = List()
}






