package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object FindLight extends Companion {
  def benchName = "find-light";
  def apply(sz: Int, parallelism: Int, what: String) = new FindLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 200000
  
  val pred = (a: Cont) => a.in < -10
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = a.in < -10
  }
}

class FindLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Option[Cont]] {
  def companion = FindLight
  runresult = None
  
  def runpar = runresult = pa.find(FindLight.pred)
  def runseq = runresult = sequentialFind(FindLight.pred, sz)
  def runjsr = runresult = { jsrarr.withFilter(FindLight.predjsr).size > 0; None }
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}























