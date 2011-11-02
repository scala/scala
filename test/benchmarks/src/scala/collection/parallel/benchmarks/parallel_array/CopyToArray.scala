package scala.collection.parallel.benchmarks.parallel_array




object CopyToArray extends Companion {
  def benchName = "copytoarray";
  def apply(sz: Int, parallelism: Int, what: String) = new CopyToArray(sz, parallelism, what)
  override def comparisons = List()
  override def defaultSize = 200000
}

class CopyToArray(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = CopyToArray
  val destarr = new Array[Any](sz)
  
  def runpar = pa.copyToArray(destarr, 0, sz)
  def runseq = sequentialCopyToArray(destarr, 0, sz)
  def comparisonMap = collection.Map()
}
