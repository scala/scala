package scala.collection.parallel
package benchmarks.parallel_view



import scala.collection.parallel.benchmarks.generic._
import scala.collection.SeqView










trait DummyViewBenches
extends ParSeqViewBench[Dummy, ParSeqView[Dummy, ParSeq[Dummy], Seq[Dummy]], Seq[Dummy]] {
  def nameOfCollection = "ParallelView"
  def operators = DummyOperators
  def comparisonMap = collection.Map()
  val forkJoinPool = new scala.concurrent.forkjoin.ForkJoinPool
  def createSequential(sz: Int, p: Int) = {
    val s = new Array[Dummy](sz)
    for (i <- 0 until sz) s(i) = new Dummy(i)
    s
  }
  def createParallel(sz: Int, p: Int) = {
    val pa = new collection.parallel.mutable.ParArray[Dummy](sz)
    forkJoinPool.setParallelism(p)
    for (i <- 0 until sz) pa(i) = new Dummy(i)
    val v = pa.view
    v.environment = forkJoinPool
    v
  }
}


object DummyViewBenchList extends DummyViewBenches with NotBenchmark










