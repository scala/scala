package scala.collection.parallel.benchmarks.parallel_array




object ReduceList extends Companion {
  def benchName = "reduce-list";
  def apply(sz: Int, p: Int, what: String) = new ReduceList(sz, p, what)
  override def comparisons = List("jsr")
  override def defaultSize = 20000
}

object ListCreator extends (Int => List[Int]) {
  def apply(idx: Int) = {
    val len = 50 + idx % 100
    (for (i <- 0 until len) yield i).toList
  }
}

object ListOps {
  val redop = (a: List[Int], b: List[Int]) => combineLists(a, b)
  val reducer = new extra166y.Ops.Reducer[List[Int]] {
    def op(a: List[Int], b: List[Int]) = combineLists(a, b)
  }
  def combineLists(a: List[Int], b: List[Int]) = {
    if (a.foldLeft(0)(_ + _) > b.foldLeft(0)(_ + _)) a else b
  }
}

class ReduceList(sz: Int, p: Int, what: String)
extends Resettable[List[Int]](sz, p, what, ListCreator, new Array[Any](_), classOf[List[Int]]) {
  def companion = ReduceList
  override def repetitionsPerRun = 10
  override val runs = 15
  
  def runpar = pa.reduce(ListOps.redop)
  def runseq = sequentialReduce(ListOps.redop, sz, List[Int]())
  def runjsr = jsrarr.reduce(ListOps.reducer, List[Int]())
  override def comparisonMap = collection.Map("jsr" -> runjsr _)
}













