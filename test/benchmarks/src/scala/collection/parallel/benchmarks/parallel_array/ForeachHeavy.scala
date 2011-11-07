package scala.collection.parallel.benchmarks.parallel_array




object ForeachHeavy extends Companion {
  def benchName = "foreach-heavy";
  def apply(sz: Int, parallelism: Int, what: String) = new ForeachHeavy(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 2048
  
  @volatile var z = 0
  
  val fun = (a: Cont) => heavyOperation(a)
  val funjsr = new extra166y.Ops.Procedure[Cont] {
    def op(a: Cont) = heavyOperation(a)
  }
  
  def heavyOperation(a: Cont) {
    checkPrime(a.in + 1000000000)
  }
  
  def checkPrime(n: Int) = {
    var isPrime = true
    var i = 2
    val until = 550
    while (i < until) {
      if (n % i == 0) isPrime = false
      i += 1
    }
    if (isPrime && (n.toString == z)) z += 1
    isPrime
  }
}

class ForeachHeavy(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ForeachHeavy
  override def repetitionsPerRun = 250
  
  def runpar = pa.pforeach(ForeachHeavy.fun)
  def runseq = sequentialForeach(ForeachHeavy.fun, sz)
  def runjsr = jsrarr.apply(ForeachHeavy.funjsr)
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}
