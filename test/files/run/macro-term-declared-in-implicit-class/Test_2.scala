// scalac: -language:experimental.macros
object Test extends App {
  import Macros._
  println("2".toOptionOfInt)
}
