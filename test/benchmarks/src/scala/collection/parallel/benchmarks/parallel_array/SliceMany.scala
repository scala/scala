package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object SliceMany extends Companion {
  def benchName = "slice-many";
  def apply(sz: Int, parallelism: Int, what: String) = new SliceMany(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 50000
}

class SliceMany(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = SliceMany
  override def repetitionsPerRun = 200
  runresult = -1
  
  def runpar = runresult = pa.slice(pa.size / 4, pa.size * 3 / 4).size
  def runseq = runresult = sequentialSlice(sz / 4, sz * 3 / 4, sz).size
  def comparisonMap = collection.Map()
}























