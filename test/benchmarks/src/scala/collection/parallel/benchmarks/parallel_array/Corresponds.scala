package scala.collection.parallel.benchmarks
package parallel_array







class Corresponds(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont])
with HavingResult[Boolean] {
  def companion = Corresponds
  override def repetitionsPerRun = 400
  
  val same = {
    val p = new collection.parallel.mutable.ParArray[Cont](sz)
    for (i <- 0 until sz) p(i) = what match {
      case "seq" => arr(i).asInstanceOf[Cont]
      case "par" => pa(i)
    }
    p
  }
  
  def runpar = runresult = pa.corresponds(same)(corr)
  def runseq = runresult = sequentialCorresponds(same, corr, sz)
  override def comparisonMap = collection.Map()
  
  val corr = (a: Cont, b: Cont) => a.in == b.in
}

object Corresponds extends Companion {
  def benchName = "corresponds";
  def apply(sz: Int, p: Int, what: String) = new Corresponds(sz, p, what)
  override def comparisons = List()
}











