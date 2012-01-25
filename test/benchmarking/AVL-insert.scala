package scala.collection





class Dummy(val a: Int) extends math.Ordered[Dummy] {
  def compare(other: Dummy) = this.a - other.a
  override def toString = a.toString
}


object Global {
  val sz = 500000
  val data = (0 until sz) map { new Dummy(_) } toArray
}


import Global._


object AVL extends testing.Benchmark {
  
  def run() {
    val avl = new collection.mutable.TreeSet[Dummy]
    
    var i = 0
    while (i < sz) {
      val elem = data(i)
      avl += elem
      i += 1
    }
  }
  
}


object ImmutableTreeSet extends testing.Benchmark {
  
  def run() {
    var tree = new collection.immutable.TreeSet[Dummy]
    
    var i = 0
    while (i < sz) {
      val elem = data(i)
      tree += elem
      i += 1
    }
  }
  
}


object JavaTreeSet extends testing.Benchmark {
  
  def run() {
    val tree = new java.util.TreeSet[Dummy]
    
    var i = 0
    while (i < sz) {
      val elem = data(i)
      tree add elem
      i += 1
    }
  }
  
}
