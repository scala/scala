object Test {  
  def main(args: Array[String]): Unit = {
    val range = 1 to 10
    val target = (3 to 8).toList
    val confirm = (xs: Seq[Int]) => assert(xs.toList == target, xs)
    
    confirm(range drop 2 dropRight 2)
    confirm(range drop 1 dropRight 1 drop 1 dropRight 1)
    confirm(range take 8 drop 2)
    confirm(range takeRight 8 dropRight 2)
    confirm(range drop 2 take 6)
    confirm(range dropRight 1 take 8 takeRight 7 drop 1)
  }
}
