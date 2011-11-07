package scala.collection.parallel.benchmarks.parallel_array




object CountList extends Companion {
  def benchName = "count-list";
  def apply(sz: Int, parallelism: Int, what: String) = new CountList(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 1000
  
  val listCreator = (i: Int) => (0 until (i % 50 + 50)).toList
  val pred = (lst: List[Int]) => check(lst)
  val predjsr = new extra166y.Ops.Predicate[List[Int]] {
    def op(lst: List[Int]) = check(lst)
  }
  
  def check(lst: List[Int]) = lst.foldLeft(0)((sum, n) => sum + n * n) % 2 == 0
}

class CountList(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, CountList.listCreator, new Array[Any](_), classOf[List[Int]]) {
  def companion = CountList
  override def repetitionsPerRun = 250
  
  def runpar = pa.count(CountList.pred)
  def runseq = sequentialCount(CountList.pred, sz)
  def runjsr = jsrarr.withFilter(CountList.predjsr).size
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}
