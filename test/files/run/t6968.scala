object Test {
  def main(args: Array[String]) {
    val mixedList = List(1,(1,2),4,(3,1),(5,4),6)
    val as = for((a,b) <- mixedList) yield a
    println(as.mkString(", "))
  }
}
