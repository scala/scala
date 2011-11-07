package scala.collection.parallel.benchmarks.parallel_array





/** Tests reduce method using an operator creating an object as a result. */
class ReduceNew(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i), 
    new Array[Any](_), classOf[Cont]) {
  def companion = ReduceNew
  override def repetitionsPerRun = 200
  
  def runpar = pa.reduce(Cont.opnew)
  def runseq = sequentialReduce(Cont.opnew, sz, new Cont(0))
  def runjsr = jsrarr.reduce(Cont.reducernew, new Cont(0))
  override def comparisonMap = collection.Map("jsr" -> runjsr _)
}

object ReduceNew extends Companion {
  def benchName = "reduce-new";
  def apply(sz: Int, p: Int, what: String) = new ReduceNew(sz, p, what)
  override def comparisons = List("jsr")
}






