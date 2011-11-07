package scala.collection.parallel.benchmarks.parallel_array





/** Tests reduce method using an operator creating an object as a result. */
class SumLight(sz: Int, p: Int, what: String)
extends Resettable[Int](sz, p, what, (i: Int) => i,  new Array[Any](_), classOf[Int]) {
  def companion = SumLight
  override def repetitionsPerRun = 500
  
  def runpar = pa.sum
  def runseq = sequentialSum(sz)
  override def comparisonMap = collection.Map()
}

object SumLight extends Companion {
  def benchName = "sum-light";
  def apply(sz: Int, p: Int, what: String) = new SumLight(sz, p, what)
  override def comparisons = List()
}






