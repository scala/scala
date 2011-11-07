package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._
import scala.collection.parallel.mutable.ParArray


object ScanLight extends Companion {
  def benchName = "scan-light";
  def apply(sz: Int, parallelism: Int, what: String) = new ScanLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 40000
  
  val op = (a: Cont, b: Cont) => {
    operation(a, b)
  }
  def operation(a: Cont, b: Cont) = {
    val m = if (a.in < 0) 1 else 0
    new Cont(a.in + b.in + m * (0 until 2).reduceLeft(_ + _))
  }
}


class ScanLight(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ScanLight
  override def repetitionsPerRun = 50
  override val runs = 12
  
  def runpar = pa.scan(new Cont(0))(ScanLight.op)
  def runseq = sequentialScan(new Cont(0), ScanLight.op, sz)
  def runjsr = jsrarr.cumulate(new extra166y.Ops.Reducer[Cont] {
    def op(a: Cont, b: Cont) = ScanLight.operation(a, b)
  }, new Cont(0))
  override def comparisonMap = collection.Map("jsr" -> runjsr _)
}










