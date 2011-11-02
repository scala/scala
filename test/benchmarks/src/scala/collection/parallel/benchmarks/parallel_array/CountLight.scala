package scala.collection.parallel.benchmarks.parallel_array




object CountLight extends Companion {
  def benchName = "count-light";
  def apply(sz: Int, parallelism: Int, what: String) = new CountLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 200000
}

class CountLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = CountLight
  
  def runpar = pa.count(Cont.pred)
  def runseq = sequentialCount(Cont.pred, sz)
  def runjsr = jsrarr.withFilter(Cont.predjsr).size
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}

