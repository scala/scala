package scala.collection.parallel.benchmarks.parallel_array




object ForallLight extends Companion {
  def benchName = "forall-light";
  def apply(sz: Int, parallelism: Int, what: String) = new ForallLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 200000
  
  val pred = (a: Cont) => a.in >= 0
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = a.in >= 0
  }
}

class ForallLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ForallLight
  
  def runpar = pa.forall(ForallLight.pred)
  def runseq = sequentialForall(ForallLight.pred, sz)
  def runjsr = jsrarr.withFilter(ForallLight.predjsr).size == sz
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}




















