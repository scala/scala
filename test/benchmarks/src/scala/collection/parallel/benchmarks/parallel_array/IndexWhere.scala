package scala.collection.parallel.benchmarks
package parallel_array







class IndexWhere(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = IndexWhere
  override def repetitionsPerRun = 400
  
  def runpar = runresult = pa.indexWhere(IndexWhere.pred2, 0)
  def runseq = runresult = sequentialIndexWhere(IndexWhere.pred2, 0, sz)
  override def comparisonMap = collection.Map()
}

object IndexWhere extends Companion {
  def benchName = "index-where";
  def apply(sz: Int, p: Int, what: String) = new IndexWhere(sz, p, what)
  override def comparisons = List()
  
  val pred = (c: Cont) => {
    var in = c.in
    var i = 2
    while (i < 5) {
      if (in % i == 0) in = 0
      i += 1
    }
    c.in >= 0 && in == -1
  }
  val pred2 = (c: Cont) => c.in == 280000
}











