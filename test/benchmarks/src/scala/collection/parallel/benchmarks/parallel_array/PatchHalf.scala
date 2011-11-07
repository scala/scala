package scala.collection.parallel.benchmarks
package parallel_array







class PatchHalf(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = PatchHalf
  override def repetitionsPerRun = 400
  
  val similar = {
    val p = new collection.parallel.mutable.ParArray[Cont](sz)
    for (i <- 0 until sz) p(i) = what match {
      case "seq" => arr(i).asInstanceOf[Cont]
      case "par" => pa(i)
    }
    p.drop(p.size / 2)
  }
  
  def runpar = runresult = pa.patch(size / 2, similar, 0).size
  def runseq = runresult = sequentialPatch(size / 2, similar, 0, size).size
  override def comparisonMap = collection.Map()
}

object PatchHalf extends Companion {
  def benchName = "patch-half";
  def apply(sz: Int, p: Int, what: String) = new PatchHalf(sz, p, what)
  override def comparisons = List()
  override def defaultSize = 25000
}











