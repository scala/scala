package scala.collection.parallel.benchmarks
package parallel_array







class SameElementsLong(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont])
with HavingResult[Boolean] {
  def companion = SameElementsLong
  override def repetitionsPerRun = 400
  
  val same = {
    val p = new collection.parallel.mutable.ParArray[Cont](sz)
    for (i <- 0 until sz) p(i) = what match {
      case "seq" => arr(i).asInstanceOf[Cont]
      case "par" => pa(i)
    }
    p
  }
  
  def runpar = runresult = pa.sameElements(same)
  def runseq = runresult = sequentialSameElements(same, sz)
  override def comparisonMap = collection.Map()
}

object SameElementsLong extends Companion {
  def benchName = "same-elements-long";
  def apply(sz: Int, p: Int, what: String) = new SameElementsLong(sz, p, what)
  override def comparisons = List()
}











