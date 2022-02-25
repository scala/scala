// scalac: -Xsource:3
object Test extends App {
  val ss =
     f"%2$$d${42}%s"    // error: Argument index out of range
  :: f"%3$$d${List(42,17): _*}%s"    // error: Argument index out of range
  :: Nil

  ss.foreach(println)
}
