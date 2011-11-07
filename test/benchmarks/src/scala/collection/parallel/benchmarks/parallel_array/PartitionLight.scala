package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object PartitionLight extends Companion {
  def benchName = "partition-light";
  def apply(sz: Int, parallelism: Int, what: String) = new PartitionLight(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 20000
  
  val pred = (a: Cont) => check(a.in)
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = check(a.in)
  }
  
  def check(n: Int) = {
    var res = n
    var i = 1
    while (i < 5) {
      res += n % i
      i += 1
    }
    (res % 2 == 0) && (res % 312 == 0)
  }
}

class PartitionLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = PartitionLight
  runresult = -1
  
  def runpar = runresult = pa.partition(PartitionLight.pred)._1.size
  def runseq = runresult = sequentialPartition(PartitionLight.pred, sz)._1.size
  def comparisonMap = collection.Map()
}























