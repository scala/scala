package scala.collection.parallel.benchmarks.arrays


import scala.collection.parallel.benchmarks._



object IntAccess extends BenchCompanion {
  def collectionName = "array";
  def benchName = "access-int";
  def apply(sz: Int, p: Int, what: String) = new IntAccess(sz, p, what)
  override def comparisons = List("any", "cast", "manif", "unknown")
  override def defaultSize = 100000
}


class IntAccess(sz: Int, p: Int, what: String)
extends Resetting(n => n, sz, p, what) with UnknownManif[Int] {
  def companion = IntAccess
  
  def runseq {}
  def runpar {}
  
  def runany = {
    var i = 0
    while (i < sz) {
      val d = anyarray(i).asInstanceOf[Int]
      i += 1
    }
  }
  
  def runcast = {
    var i = 0
    while (i < sz) {
      val d = Arrays.apply(castarray, i).asInstanceOf[Int]
      i += 1
    }
  }
  
  def runmanif = {
    var i = 0
    while (i < sz) {
      val d = manifarray(i)
      if (op(d)) i += 1
      i += 1
    }
  }
  
  def op(a: Int) = a < 0
  
  def comparisonMap = collection.Map("any" -> runany _, "cast" -> runcast _,
                                     "manif" -> runmanif _, "unknown" -> rununknown _)
  
}














