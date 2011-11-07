package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object FilterLight extends Companion {
  def benchName = "filter-light";
  def apply(sz: Int, parallelism: Int, what: String) = new FilterLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 10000
  
  val pred = (a: Cont) => check(a.in)
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = check(a.in)
  }
  
  def check(n: Int) = {
    var res = n
//    var i = 1
//    while (i < 10) {
//      res += n % i
//      i += 1
//    }
    res % 2 == 0
  }
}

class FilterLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = FilterLight
  override def repetitionsPerRun = 250
  override val runs = 30
  runresult = -1
  
  def runpar = runresult = pa.filter(FilterLight.pred).size
  def runseq = runresult = sequentialFilter(FilterLight.pred, sz).size
  def runjsr = runresult = { jsrarr.withFilter(FilterLight.predjsr).all.size }
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}























