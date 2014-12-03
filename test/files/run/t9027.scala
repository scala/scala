
// used to be parsed as .println
object Test extends App {
  import reflect.runtime._, universe._

  val trees = List(
  q"""<a/><b/>
      println("hello, world.")""",
  q"""<a/>
      <b/>
      <c/>
      println("hello, world.")"""
  )
  trees foreach println
}
