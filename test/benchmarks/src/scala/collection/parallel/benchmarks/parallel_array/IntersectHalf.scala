package scala.collection.parallel.benchmarks
package parallel_array







class IntersectHalf(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, (i: Int) => new Cont(i),  new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = IntersectHalf
  override def repetitionsPerRun = 400
  
  val similar = {
    val p = new collection.parallel.mutable.ParArray[Cont](sz)
    for (i <- 0 until sz) p(i) = what match {
      case "seq" => arr(i).asInstanceOf[Cont]
      case "par" => pa(i)
    }
    p.drop(p.size / 2)
  }
  
  def runpar = runresult = pa.intersect(similar).size
  def runseq = runresult = sequentialIntersect(similar, sz).size
  override def comparisonMap = collection.Map()
  
  val corr = (a: Cont, b: Cont) => a.in == b.in
}

object IntersectHalf extends Companion {
  def benchName = "intersect-half";
  def apply(sz: Int, p: Int, what: String) = new IntersectHalf(sz, p, what)
  override def comparisons = List()
  override def defaultSize = 10000
}











