// scalac: -Xsource:3
object Test extends App {
  val ss =
     f"%1$$d${42}"        // 4242
  :: f"%1$$d${42}%s"      // same
  :: f"%2$$d${42}${17}"   // 174217
  :: f"%2$$d${List(42,17): _*}%s"    // 1742, was: error: Argument index out of range
  :: Nil

  ss.foreach(println)
}
