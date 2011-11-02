package scala.collection.parallel.benchmarks.parallel_array




object ForeachLight extends Companion {
  def benchName = "foreach-light";
  def apply(sz: Int, parallelism: Int, what: String) = new ForeachLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 200000
  
  val fun = (a: Cont) => a.num = a.in
  val funjsr = new extra166y.Ops.Procedure[Cont] {
    def op(a: Cont) = a.num = a.in
  }
}

class ForeachLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ForeachLight
  
  def runpar = pa.pforeach(ForeachLight.fun)
  def runseq = sequentialForeach(ForeachLight.fun, sz)
  def runjsr = jsrarr.apply(ForeachLight.funjsr)
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}
