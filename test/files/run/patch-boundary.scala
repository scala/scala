object Test {
  def f = collection.mutable.ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8)
  def g = f.patch(4, List(1, 2), 10)
  
  def main(args: Array[String]): Unit = {
    assert(g.size == 6)    
  }
}
