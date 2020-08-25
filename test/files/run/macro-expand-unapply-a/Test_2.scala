// scalac: -language:experimental.macros
import Macros._

object Test extends App {
  List(1, 2) match { case UnapplyMacro(x, y) => println((x, y)) }
  List(1, 2, 3) match { case UnapplyMacro(x, y, z) => println((x, y, z)) }
}
