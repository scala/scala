

import collection._


// #4535
object Test {
  
  def main(args: Array[String]) {
    val as = new mutable.ArrayStack[Int]
    as push 1
    as push 2
    as push 3
    println(as.reverse)
    
    as push 4
    as push 5
    as push 6
    println(as.reverse)
    
    println(as map { x => x })
    
    for (i <- 0 until 100) {
      as push i
      assert(as == as.map(x => x))
      assert(as == as.reverse.reverse)
    }
  }
  
}
