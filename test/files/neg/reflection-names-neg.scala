import scala.reflect.runtime.universe._

object Test {
  val x1 = "abc" drop 1                 // "bc": String
  val x2 = ("abc": Name) drop 1         // error
}
