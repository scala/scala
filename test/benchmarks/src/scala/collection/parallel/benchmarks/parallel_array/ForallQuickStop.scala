package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object ForallQuickStop extends Companion {
  def benchName = "forall-quickstop";
  def apply(sz: Int, parallelism: Int, what: String) = new ForallQuickStop(sz, parallelism, what)
  override def defaultSize = 200000
  
  val pred = (a: Cont) => a.in != 50
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = a.in != 50
  }
}

class ForallQuickStop(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Boolean] {
  def companion = ForallQuickStop
  
  def runpar = runresult = pa.forall(ForallQuickStop.pred)
  def runseq = runresult = sequentialForall(ForallQuickStop.pred, sz)
  def comparisonMap = collection.Map()
}




















