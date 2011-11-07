package scala.collection.parallel.benchmarks
package parallel_array







class LastIndexWhere(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = LastIndexWhere
  override def repetitionsPerRun = 400
  
  def runpar = runresult = pa.lastIndexWhere(LastIndexWhere.pred2, pa.size - 1)
  def runseq = runresult = sequentialLastIndexWhere(LastIndexWhere.pred2, sz - 1, sz)
  override def comparisonMap = collection.Map()
}

object LastIndexWhere extends Companion {
  def benchName = "last-index-where";
  def apply(sz: Int, p: Int, what: String) = new LastIndexWhere(sz, p, what)
  override def comparisons = List()
  
  val pred = (c: Cont) => {
    var in = c.in
    var i = 2
    while (i < 5) {
      if (in % i == 0) in = 0
      i += 1
    }
    c.in >= 0 || in == 0
  }
  val pred2 = (c: Cont) => c.in == 500
}











