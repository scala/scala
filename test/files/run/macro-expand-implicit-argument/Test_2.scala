// scalac: -language:experimental.macros
object Test extends App {
  import Macros._
  println(array(1, 2, 3).toList)
}
