object Test {
  val f: PartialFunction[String, Int] = {
    PartialFunction.empty[String, Int] orElse {
      case "abc"  => 100
      case s      => s.length
    }
  }
  
  def main(args: Array[String]): Unit = {
    println(f("abc"))
    println(f("def")) 
    println(PartialFunction.empty[String, Int] isDefinedAt "abc")
  }
}
