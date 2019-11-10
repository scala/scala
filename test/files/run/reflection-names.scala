import scala.tools.nsc._

object Test {
  val global = new Global(new Settings())
  import global._

  val x1 = "abc" drop 1                    // "bc": String
  val x2 = NameOps.drop(TermName("abc"), 1)  // "bc": TermName
  val x3 = NameOps.drop(TypeName("abc"), 1)  // "bc": TypeName
  val x4 = NameOps.drop(TypeName("abc"), 1)  // "bc": Name

  def main(args: Array[String]): Unit = {
    List(x1, x2, x3, x4) foreach (x => println(x.getClass.getName, x))
  }
}
