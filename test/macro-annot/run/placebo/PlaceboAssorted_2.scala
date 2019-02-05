import scala.reflect.runtime.universe._

class PlaceboAssortedZoo {
  @placebo def foo(x: Int) = x
  @placebo val bar = 2
  @placebo var baz = 3
  @placebo lazy val bax = 4
  @placebo type T = Int
}

class PlaceboAssorted {
  def assertEquals(a: Any, b: Any): Unit = { assert(a == b, s"$a != $b") }

  def combo = { nested ; local }

  // @Test
  def nested: Unit = {
    assertEquals(typeOf[PlaceboAssortedZoo].decls.sorted.map(_.toString).mkString("\n"), """
      |constructor PlaceboAssortedZoo
      |method foo
      |type T
      |value bar
      |value bar
      |variable baz
      |variable baz
      |variable baz
      |lazy value bax
    """.trim.stripMargin)
  }

  // @Test
  def local: Unit = {
    @placebo def foo(x: Int) = x
    @placebo val bar = 2
    @placebo var baz = 3
    @placebo lazy val bax = 4
    @placebo type T = Int

    assertEquals(foo(1), 1)
    assertEquals(bar, 2)
    assertEquals(baz, 3)
    assertEquals(bax, 4)
    assertEquals(List[T](5), List(5))
  }
}
