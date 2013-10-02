



object Test {

  def main(args: Array[String]) {
    println(Seq(List('1', '2', '3'), List('a', 'b', 'c')).view.addString(new StringBuilder, "_"))
  }

}
