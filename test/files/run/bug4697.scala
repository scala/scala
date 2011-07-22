object Test {
  var st = Stream(0)
  for (i <- 1 to 10000) st = i +: st

  def main(args: Array[String]): Unit = {
    println(st.take(10000).sum)
  }
}
