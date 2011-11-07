package scala.collection.parallel.benchmarks
package parallel_array







class ReverseMap(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont]) {
  def companion = ReverseMap
  override def repetitionsPerRun = 100
  
  def runpar = pa.reverseMap(compl)
  def runseq = sequentialReverseMap(compl, sz)
  override def comparisonMap = collection.Map()
  
  val id = (c: Cont) => c
  val compl = (c: Cont) => {
    var in = c.in
    var i = 2
    while (i < 6) {
      if (in % i == 0) in = 0
      i += 1
    }
    if (in < 0) null
    else c
  }
}

object ReverseMap extends Companion {
  def benchName = "reverse-map";
  def apply(sz: Int, p: Int, what: String) = new ReverseMap(sz, p, what)
  override def comparisons = List()
  override def defaultSize = 100000
}











