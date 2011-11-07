

// ticked #3444
object Test {
  
  def main(args: Array[String]) {
    val it = (1 to 12).toSeq.iterator
    
    assert(it.next == 1)
    assert(it.take(2).toList == List(2, 3))
    
    val jt = (4 to 12).toSeq.iterator
    assert(jt.next == 4)
    assert(jt.drop(5).toList == List(10, 11, 12))
    
    val kt = (1 until 10).toSeq.iterator
    assert(kt.drop(50).toList == Nil)
    
    val mt = (1 until 5).toSeq.iterator
    assert(mt.take(50).toList == List(1, 2, 3, 4))
  }
  
}
