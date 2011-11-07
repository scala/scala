package scala.collection.parallel.benchmarks
package parallel_array







class Reverse(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont]) {
  def companion = Reverse
  override def repetitionsPerRun = 400
  
  def runpar = pa.reverse
  def runseq = sequentialReverse(sz)
  override def comparisonMap = collection.Map()
}

object Reverse extends Companion {
  def benchName = "reverse";
  def apply(sz: Int, p: Int, what: String) = new Reverse(sz, p, what)
  override def comparisons = List()
}











