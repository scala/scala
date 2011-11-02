package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._
import scala.collection.parallel.mutable.ParArray


object ScanMedium extends Companion {
  def benchName = "scan-medium";
  def apply(sz: Int, parallelism: Int, what: String) = new ScanMedium(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 5000
  
  val op = (a: Cont, b: Cont) => {
    operation(a, b)
  }
  def operation(a: Cont, b: Cont) = {
    val m = if (a.in < 0) 1 else 0
    val k = calc(a.in, b.in, m)
    new Cont(a.in + b.in + k * m * (0 until 2).reduceLeft(_ + _))
  }
  private def calc(x: Int, y: Int, n: Int) = {
    var sum = x
    for (i <- 0 until 500) {
      sum += y + (if (sum % 2 == 0) n * x else y)
      if (sum % 5 == 0) sum -= x * y - n * (x + y)
    }
    sum
  }
}


class ScanMedium(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ScanMedium
  override def repetitionsPerRun = 50
  override val runs = 12
  
  def runpar = pa.scan(new Cont(0))(ScanMedium.op)
  def runseq = sequentialScan(new Cont(0), ScanMedium.op, sz)
  def runjsr = jsrarr.cumulate(new extra166y.Ops.Reducer[Cont] {
    def op(a: Cont, b: Cont) = ScanMedium.operation(a, b)
  }, new Cont(0))
  override def comparisonMap = collection.Map("jsr" -> runjsr _)
}










