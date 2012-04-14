import scala.reflect.mirror._

object Test {
  def test(name: String, address: String) = null
  def main(args: Array[String]) = {
    val tree = reify((x:String) => test(address=x,name=x)).tree
    println(tree)
  }
}
