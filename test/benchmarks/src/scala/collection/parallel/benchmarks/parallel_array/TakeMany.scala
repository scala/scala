package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object TakeMany extends Companion {
  def benchName = "take-many";
  def apply(sz: Int, parallelism: Int, what: String) = new TakeMany(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 250000
}

class TakeMany(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = TakeMany
  override def repetitionsPerRun = 400
  runresult = -1
  
  def runpar = runresult = pa.take(pa.size / 2).size
  def runseq = runresult = sequentialTake(sz / 2, sz).size
  def comparisonMap = collection.Map()
}























