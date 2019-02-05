import scala.tools.partest.Util.ArrayDeep

object Test {
  def main(args: Array[String]): Unit = {
    println(args.map(_ => foo _).map(_ => "<function1>").deep)
  }

  def foo(xs: String*): Unit = {
    println(xs)
  }
}
