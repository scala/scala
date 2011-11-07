package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object SliceMedium extends Companion {
  def benchName = "slice-medium";
  def apply(sz: Int, parallelism: Int, what: String) = new SliceMedium(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 50000
}

class SliceMedium(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = SliceMedium
  override def repetitionsPerRun = 200
  runresult = -1
  
  def runpar = runresult = pa.slice(pa.size / 7, pa.size * 4 / 7).size
  def runseq = runresult = sequentialSlice(sz / 7, sz * 4 / 7, sz).size
  def comparisonMap = collection.Map()
}























