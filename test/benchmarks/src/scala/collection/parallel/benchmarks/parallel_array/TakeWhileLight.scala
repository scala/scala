package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object TakeWhileLight extends Companion {
  def benchName = "takewhile-light";
  def apply(sz: Int, parallelism: Int, what: String) = new TakeWhileLight(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 10000
  
  val pred = (a: Cont) => check(a.in)
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = check(a.in)
  }
  
  def check(n: Int) = {
    var res = n
    var i = 1
    while (i < 10) {
      res += n % i
      i += 1
    }
    res % 2 == 0 || n > 0
  }
}

class TakeWhileLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = TakeWhileLight
  runresult = -1
  
  def runpar = runresult = pa.takeWhile(TakeWhileLight.pred).size
  def runseq = runresult = sequentialTakeWhile(TakeWhileLight.pred, sz).size
  def comparisonMap = collection.Map()
}























