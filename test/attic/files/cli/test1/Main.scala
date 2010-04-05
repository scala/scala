// @info no dependency
package test1
object Main {
  def main(args: Array[String]) = {
    val arg = if (args != null && args.length > 0) args(0) else "?"
    Console.println("1: test " + arg + " passed (" + args.length + ")")
  }
}
