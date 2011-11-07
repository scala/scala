object Test {
  def f(args: Array[String]) = args match {
    case Array("-p", prefix, from, to) =>
      prefix + from + to
    
    case Array(from, to) =>
      from + to

    case _ =>
      "default"
  }
  
  def main(args: Array[String]) {
    assert(f(Array("1", "2")) == "12")
  }
}