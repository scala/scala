package scala.collection.parallel.benchmarks
package parallel_array







class RemoveDuplicates(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = RemoveDuplicates
  override def repetitionsPerRun = 400
  
  def runpar = runresult = pa.distinct.size
  def runseq = runresult = sequentialRemoveDuplicates(size).size
  override def comparisonMap = collection.Map()
}

object RemoveDuplicates extends Companion {
  def benchName = "remove-duplicates";
  def apply(sz: Int, p: Int, what: String) = new RemoveDuplicates(sz, p, what)
  override def comparisons = List()
  override def defaultSize = 10000
}


















