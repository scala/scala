package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object DropMany extends Companion {
  def benchName = "drop-many";
  def apply(sz: Int, parallelism: Int, what: String) = new DropMany(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 50000
}

class DropMany(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = DropMany
  override def repetitionsPerRun = 400
  runresult = -1
  
  def runpar = runresult = pa.drop(pa.size / 2).size
  def runseq = runresult = sequentialDrop(sz / 2, sz).size
  def comparisonMap = collection.Map()
}























