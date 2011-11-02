package scala.collection.parallel.benchmarks.parallel_array




object MapLight extends Companion {
  def benchName = "map-light";
  def apply(sz: Int, parallelism: Int, what: String) = new MapLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 100000
  
  def fun = (a: Cont) => { a }
  def funjsr = new extra166y.Ops.Op[Cont, Cont] {
    def op(a: Cont) = { a }
  }
}

class MapLight(sz: Int, p: Int, what: String)
extends Resettable(sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = MapLight
  
  def runpar = pa.map(MapLight.fun)
  def runseq = sequentialMap(MapLight.fun, sz)
//  def runseq = sequentialMapOpt(MapLight.fun, sz)
  def runjsr = jsrarr.replaceWithMapping(MapLight.funjsr).all
  def comparisonMap = collection.Map("jsr" -> runjsr _)
}
