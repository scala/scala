object Test {
  def test(name: String, address: String) = null
  def main(args: Array[String]) = {
    val tree = scala.reflect.Code.lift((x:String) => test(address=x,name=x)).tree
    println(tree)
  }
}
