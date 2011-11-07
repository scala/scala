package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object SplitHalf extends Companion {
  def benchName = "split-half";
  def apply(sz: Int, parallelism: Int, what: String) = new SplitHalf(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 50000
}

class SplitHalf(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = SplitHalf
  override def repetitionsPerRun = 300
  runresult = -1
  
  def runpar = runresult = pa.splitAt(pa.size / 2)._1.size
  def runseq = runresult = sequentialSplitAtOpt(sz / 2, sz)._1.size
  def comparisonMap = collection.Map()
}























