package scala.collection.parallel.benchmarks.arrays


import scala.collection.parallel.benchmarks._



object ObjectAccess extends BenchCompanion {
  def collectionName = "array";
  def benchName = "access-obj";
  def apply(sz: Int, p: Int, what: String) = new ObjectAccess(sz, p, what)
  override def comparisons = List("any", "cast", "gencast", "manif", "unknown")
  override def defaultSize = 100000
}


class ObjectAccess(sz: Int, p: Int, what: String)
extends Resetting(Dummy(_), sz, p, what) with UnknownManif[Dummy] {
  def companion = ObjectAccess
  
  def runseq {}
  def runpar {}
  
  def runany = {
    var i = 0
    while (i < sz) {
      val d = anyarray(i).asInstanceOf[Dummy]
      Dummy.dummyOp(d)
      i += 1
    }
  }
  
  def runcast = {
    var i = 0
    while (i < sz) {
      val d = Arrays.apply(castarray, i).asInstanceOf[Dummy]
      i += 1
    }
  }
  
  def rungenericcast = {
    var i = 0
    while (i < sz) {
      val d = Arrays.genericApply(gencastarray, i)
      i += 1
    }
  }
  
  def runmanif = {
    var i = 0
    while (i < sz) {
      val d = manifarray(i)
      if (d.in < 0) i += 1
      i += 1
    }
  }
  
  def comparisonMap = collection.Map("any" -> runany _, "cast" -> runcast _, "gencast" -> rungenericcast _,
                                     "manif" -> runmanif _, "unknown" -> rununknown _)
  
}














