package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object ExistsLight extends Companion {
  def benchName = "exists-light";
  def apply(sz: Int, parallelism: Int, what: String) = new ExistsLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 200000
  
  val pred = (a: Cont) => a.in < 0
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = a.in < 0
  }
}

class ExistsLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Boolean] {
  def companion = ExistsLight
  runresult = false
  
  def runpar = runresult = pa.exists(ExistsLight.pred)
  def runseq = runresult = sequentialExists(ExistsLight.pred, sz)
  def runjsr = runresult = jsrarr.withFilter(ExistsLight.predjsr).size > 0
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}




















