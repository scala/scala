//  b.scala
package pkg {
  package object other
  package other { class Crash { foo("") } }
}
 
object Test {
  def main(args: Array[String]): Unit = new pkg.other.Crash
}
