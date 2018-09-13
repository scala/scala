
class C {
  val c1 = "c"
  println("c")
}
trait T {
  final val t1: String = "t"
  println(t1)
}
object Test extends C with T {
  val o1: String = "o1"
  println("o1")

  def main(args: Array[String]): Unit = {
    import java.lang.reflect.{Field, Modifier}
    def checkStaticFinal(name: String, isFinal: Boolean): Field = {
      val f = Test.getClass.getDeclaredField(name)
      assert(Modifier.isStatic(f.getModifiers))
      assert(Modifier.isFinal(f.getModifiers) == isFinal)
      f
    }
    checkStaticFinal("MODULE$", isFinal = true)
    checkStaticFinal("t1", isFinal = false)
    checkStaticFinal("o1", isFinal = true)
  }
}
