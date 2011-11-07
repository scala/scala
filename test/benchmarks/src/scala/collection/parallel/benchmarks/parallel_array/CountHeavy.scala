package scala.collection.parallel.benchmarks.parallel_array




object CountHeavy extends Companion {
  def benchName = "count-heavy";
  def apply(sz: Int, parallelism: Int, what: String) = new CountHeavy(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 16
  
  val pred = (a: Cont) => heavyCheck(a)
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = heavyCheck(a)
  }
  
  def heavyCheck(a: Cont) = {
    val n = a.in
    (n until (n + 200)).map(checkPrime(_)).reduceLeft(_ && _)
  }
  def checkPrime(n: Int) = {
    var isPrime = true
    for (i <- 2 until (scala.math.sqrt(n).toInt + 1)) if (n % i == 0) isPrime = false
    isPrime
  }
}

class CountHeavy(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = CountHeavy
  
  def runpar = pa.count(CountHeavy.pred)
  def runseq = sequentialCount(CountHeavy.pred, sz)
  def runjsr = jsrarr.withFilter(CountHeavy.predjsr).size
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}
