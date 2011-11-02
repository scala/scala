package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object ForallStop80k extends Companion {
  def benchName = "forall-stop80k";
  def apply(sz: Int, parallelism: Int, what: String) = new ForallStop80k(sz, parallelism, what)
  override def defaultSize = 100000
  
  val pred = (a: Cont) => a.in != 80000
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = a.in != 80000
  }
}

class ForallStop80k(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Boolean] {
  def companion = ForallStop80k
  
  def runpar = runresult = pa.forall(ForallStop80k.pred)
  def runseq = runresult = sequentialForall(ForallStop80k.pred, sz)
  def comparisonMap = collection.Map()
}




















