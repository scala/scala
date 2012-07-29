import scala.tools.nsc._

object Test {
  val global = new Global(new Settings())
  import global._

  val x1 = "abc" drop 1                      // "bc": String
  val x2 = ("abc": TermName) drop 1          // "bc": TermName
  val x3 = ("abc": TypeName) drop 1          // "bc": TypeName
  val x4 = (("abc": TypeName): Name) drop 1  // "bc": Name

  def main(args: Array[String]): Unit = {
    List(x1, x2, x3, x4) foreach (x => println(x.getClass.getName, x))
  }
}
