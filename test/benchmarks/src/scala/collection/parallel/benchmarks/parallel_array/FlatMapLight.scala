package scala.collection.parallel.benchmarks.parallel_array






object FlatMapLight extends Companion {
  def benchName = "flatmap-light";
  def apply(sz: Int, parallelism: Int, what: String) = new FlatMapLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 10000
  
  def fun = (a: Cont) => { List(1, 2, 3, 4, a.in) }
}

class FlatMapLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = FlatMapLight
  
  def runpar = pa.flatMap(FlatMapLight.fun)
  def runseq = sequentialFlatMap(FlatMapLight.fun, sz)
  def comparisonMap = collection.Map()
}
