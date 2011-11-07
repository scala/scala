package scala.collection.parallel.benchmarks
package parallel_array



object GroupByLight extends Companion {
  def benchName = "groupby-light";
  def apply(sz: Int, parallelism: Int, what: String) = new GroupByLight(sz, parallelism, what)
  override def comparisons = List()
  override def defaultSize = 10000
  
  val fun = (a: Cont) => a.in % 32
}


class GroupByLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont])
with HavingResult[Int] {
  def companion = GroupByLight
  runresult = -1
  
  val array = new Array[Cont](sz)
  for (i <- 0 until sz) array(i) = new Cont(i)
  
  def runpar = runresult = pa.groupBy(GroupByLight.fun).size
  def runseq = runresult = array.asInstanceOf[Array[Cont]].groupBy(GroupByLight.fun).size
  def comparisonMap = collection.Map()
}

















