package scala.collection.parallel.benchmarks.parallel_array




object PartialMapLight extends Companion {
  def benchName = "partmap-light";
  def apply(sz: Int, parallelism: Int, what: String) = new PartialMapLight(sz, parallelism, what)
  override def comparisons = List()
  override def defaultSize = 100000
  
  def fun: PartialFunction[Cont, Cont] = {
    case c: Cont if c.in >= 0 => c
  }
}

class PartialMapLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = PartialMapLight
  
  def runpar = pa.collect(PartialMapLight.fun)
  def runseq = sequentialPartialMap(PartialMapLight.fun, sz)
  def comparisonMap = collection.Map()
}
