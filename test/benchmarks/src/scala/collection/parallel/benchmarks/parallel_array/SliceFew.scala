package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object SliceFew extends Companion {
  def benchName = "slice-few";
  def apply(sz: Int, parallelism: Int, what: String) = new SliceFew(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 50000
}

class SliceFew(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = SliceFew
  override def repetitionsPerRun = 200
  runresult = -1
  
  def runpar = runresult = pa.slice(5, 25).size
  def runseq = runresult = sequentialSlice(5, 25, sz).size
  def comparisonMap = collection.Map()
}























