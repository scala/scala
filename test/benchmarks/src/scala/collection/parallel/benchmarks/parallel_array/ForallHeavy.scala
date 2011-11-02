package scala.collection.parallel.benchmarks.parallel_array




object ForallHeavy extends Companion {
  def benchName = "forall-heavy";
  def apply(sz: Int, parallelism: Int, what: String) = new ForallHeavy(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 16
  
  val pred = (a: Cont) => heavyCheck(a)
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = heavyCheck(a)
  }
  
  def heavyCheck(a: Cont) = {
    val init = a.in + 1
    var cnt = init
    var i = 0
    while (i < 10000) {
      cnt = -2 * cnt
      cnt /= 2
      i += 1
    }
    cnt += init * 5 + 10
    cnt >= 0
  }
}

class ForallHeavy(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ForallHeavy
  
  def runpar = pa.forall(ForallHeavy.pred)
  def runseq = sequentialForall(ForallHeavy.pred, sz)
  def runjsr = jsrarr.withFilter(ForallHeavy.predjsr).size == sz
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}




















