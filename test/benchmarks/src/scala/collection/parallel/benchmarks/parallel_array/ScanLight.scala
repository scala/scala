package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._
import scala.collection.parallel.mutable.ParArray


object ScanLight extends Companion {
  def benchName = "scan-light";
  def apply(sz: Int, parallelism: Int, what: String) = new ScanLight(sz, parallelism, what)
  override def comparisons = List()
  override def defaultSize = 40000

  val op = (a: Cont, b: Cont) => {
    val m = if (a.in < 0) 1 else 0
    new Cont(a.in + b.in + m * (0 until 10).reduceLeft(_ + _))
  }
}


class ScanLight(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ScanLight
  override def repetitionsPerRun = 10
  override val runs = 10

  def runpar = pa.scan(new Cont(0))(ScanLight.op)
  def runseq = sequentialScan(new Cont(0), ScanLight.op, sz)
  override def comparisonMap = collection.Map()
}










