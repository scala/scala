package scala.collection.parallel.benchmarks.parallel_array




class ReduceHeavy(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ReduceHeavy
  override def repetitionsPerRun = 100
  
  def runseq = sequentialReduce(Cont.opheavy, sz, new Cont(0))
  def runpar = pa.reduce(Cont.opheavy)
  def runjsr = jsrarr.reduce(Cont.reducerheavy, new Cont(0))
  override def comparisonMap = collection.Map("jsr" -> runjsr _)
}

object ReduceHeavy extends Companion {
  def benchName = "reduce-heavy";
  def apply(sz: Int, p: Int, what: String) = new ReduceHeavy(sz, p, what)
  override def comparisons = List("jsr")
  override def defaultSize = 16
}
