package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.mutable.ParArray


object PlusPlus extends Companion {
  def benchName = "plusplus";
  def apply(sz: Int, parallelism: Int, what: String) = new PlusPlus(sz, parallelism, what)
  override def comparisons = List()
  override def defaultSize = 50000
}

class PlusPlus(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = PlusPlus
  
  val thatarr = new Array[Cont](sz)
  val thatpa = new ParArray[Cont](sz)
  
  def runpar = pa ++ thatpa
  def runseq = arr ++ thatarr
  def comparisonMap = collection.Map()
}





