package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object SpanLight extends Companion {
  def benchName = "span-light";
  def apply(sz: Int, parallelism: Int, what: String) = new SpanLight(sz, parallelism, what)
  override def comparisons = Nil
  override def defaultSize = 20000
  
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
    if (n != 10000) res % 2 == 0 || n != 10000
    else false
  }
}

class SpanLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = SpanLight
  runresult = -1
  
  def runpar = runresult = pa.span(SpanLight.pred)._1.size
  def runseq = runresult = sequentialSpan(SpanLight.pred, sz)._1.size
  def comparisonMap = collection.Map()
}























