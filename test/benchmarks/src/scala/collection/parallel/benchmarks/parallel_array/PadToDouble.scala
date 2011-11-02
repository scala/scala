package scala.collection.parallel.benchmarks
package parallel_array







class PadToDouble(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = PadToDouble
  override def repetitionsPerRun = 400
  
  val similar = {
    val p = new collection.parallel.mutable.ParArray[Cont](sz)
    for (i <- 0 until sz) p(i) = what match {
      case "seq" => arr(i).asInstanceOf[Cont]
      case "par" => pa(i)
    }
    p.drop(p.size / 2)
  }
  
  def runpar = runresult = pa.padTo(size * 2, padder).size
  def runseq = runresult = sequentialPadTo(size * 2, padder, size).size
  override def comparisonMap = collection.Map()
  
  val padder = new Cont(0)
}


object PadToDouble extends Companion {
  def benchName = "padto-double";
  def apply(sz: Int, p: Int, what: String) = new PadToDouble(sz, p, what)
  override def comparisons = List()
  override def defaultSize = 25000
}















