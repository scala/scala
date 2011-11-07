package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._


object IntWrapCreator extends (Int => IntWrap) {
  def apply(idx: Int) = new IntWrap(shiftaround(idx))
  def shiftaround(idx: Int) = idx * 40192 + 717
}

case class IntWrap(val num: Int)

object IntOps {
  val op = (a: IntWrap, b: IntWrap) => primereduce(a, b)
  val reducer = new extra166y.Ops.Reducer[IntWrap] {
    def op(a: IntWrap, b: IntWrap) = primereduce(a, b)
  }
  
  def primereduce(a: IntWrap, b: IntWrap) = {
    val check = (checkPrime(a.num), checkPrime(b.num))
    if (a.num > b.num) a else b
  }
  
  def checkPrime(n: Int) = {
    var isPrime = true
    var i = 2
    val until = scala.math.sqrt(n).toInt + 1
    while (i < until) {
      if (n % i == 0) isPrime = false
      i += 1
    }
    isPrime
  }
}

class ReducePrime(sz: Int, p: Int, what: String)
extends Resettable[IntWrap](sz, p, what, IntWrapCreator, new Array[Any](_), classOf[IntWrap])
with HavingResult[IntWrap] {
  def companion = ReducePrime
  
  def runseq = runresult = sequentialReduce(IntOps.op, sz, new IntWrap(0))
  def runpar = runresult = pa.reduce(IntOps.op)
  def runjsr = runresult = jsrarr.reduce(IntOps.reducer, new IntWrap(0))
  override def comparisonMap = collection.Map("jsr" -> runjsr _)
}

object ReducePrime extends Companion {
  def benchName = "reduce-prime";
  def apply(sz: Int, p: Int, what: String) = new ReducePrime(sz, p, what)
  override def comparisons = List("jsr")
  override def defaultSize = 100
}












