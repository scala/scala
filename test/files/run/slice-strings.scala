object Test {  
  def cmp(x1: String) = {
    val x2 = x1.toList
    
    -10 to 10 foreach { i =>
      assert(x1.take(i) == x2.take(i).mkString)
      assert(x1.drop(i) == x2.drop(i).mkString)
      assert(x1.takeRight(i) == x2.takeRight(i).mkString)
      assert(x1.dropRight(i) == x2.dropRight(i).mkString)
    }
    for (idx1 <- -3 to 3 ; idx2 <- -3 to 3) {
      assert(x1.slice(idx1, idx2) == x2.slice(idx1, idx2).mkString)
    }
  }
  
  def main(args: Array[String]): Unit = {
     cmp("abcde")
  }
}
