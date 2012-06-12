import scala.reflect.runtime.universe._
import scala.tools.reflect.Eval

object Test extends App {
  reify {
    val RegexParser = """(.*) \d+([A-Z]+) \| (.*) \|.*""".r
    val RegexParser(name, shortname, value) = "American Dollar 1USD | 2,8567 | sometext"
    println("name = %s, shortname = %s, value = %s".format(name, shortname, value))
  }.eval
}