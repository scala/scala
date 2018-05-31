import scala.reflect.runtime.universe._

class PlaceboParameterZoo {
  class C[@placebo T](@placebo val x: Int)
  object С
  def m[@placebo T, @placebo U](@placebo x: Int)(@placebo y: Int) = ???
  type T[@placebo U] = U
}

class PlaceboParameters {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  // @Test
  def combo: Unit = {
    assertEquals(typeOf[PlaceboParameterZoo].decls.sorted.map(_.toString).mkString("\n"), """
      |constructor PlaceboParameterZoo
      |class C
      |object С
      |method m
      |type T
    """.trim.stripMargin)
  }
}
