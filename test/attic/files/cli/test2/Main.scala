// @info 1 dependency
package test2 
object Main {
  def main(args: Array[String]) = {
    Console.print("2: ")
    test1.Main.main(args)
  }
}
