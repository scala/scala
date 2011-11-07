package scala.collection.parallel.benchmarks.arrays




trait UnknownManif[T] {
  def manifarray: Array[T]
  def size: Int
  
  def rununknown {
    val arr = manifarray
    val sz = size
    var i = 0
    while (i < sz) {
      val d = arr(i)
      op(d)
      i += 1
    }
  }
  
  def op(d: Any) {}
}
















