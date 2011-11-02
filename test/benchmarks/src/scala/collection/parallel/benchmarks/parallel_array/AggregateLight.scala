package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._
import scala.collection.parallel.mutable.ParArray
import extra166y.{ParallelArray => JSR166Array}


object AggregateLight extends Companion {
  def benchName = "aggregate-light";
  def apply(sz: Int, parallelism: Int, what: String) = new AggregateLight(sz, parallelism, what)
  override def comparisons = List()
  override def defaultSize = 200000
  
  val seqop = (a: Cont, b: Cont) => b
  val combop = (a: Cont, b: Cont) => a
}


class AggregateLight(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = AggregateLight
  override def repetitionsPerRun = 350
  override val runs = 20
  
  def runpar = pa.aggregate(new Cont(0))(companion.seqop, companion.combop)
  def runseq = sequentialReduce(companion.seqop, sz, new Cont(0))
  override def comparisonMap = collection.Map()
}










