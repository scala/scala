import scala.reflect.runtime.universe._

class ParameterZoo {
  class C[@funny T](@funny val x: Int)
  object С
  def m[@funny T, @funny U](@funny x: Int)(@funny y: Int) = ???
  type T[@funny U] = U
}

object Test extends App {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  // @Test  def combo: Unit =
  {
    assertEquals(typeOf[ParameterZoo].decls.sorted.map(_.toString).mkString("\n"), """
      |constructor ParameterZoo
      |object С
      |class CTx
      |method mTUxy
      |type TU
    """.trim.stripMargin)
  }
}
