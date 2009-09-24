// @info 2 dependencies
package test3
object Main {
  def main(args: Array[String]) = {
    Console.print("3: ")
    test1.Main.main(args)
    Console.print("3: ")
    test2.Main.main(args)
  }
}
