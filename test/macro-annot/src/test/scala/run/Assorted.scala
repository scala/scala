import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._
import scala.reflect.runtime.universe._

class AssortedZoo {
  @doubler def foo(x: Int) = x
  @doubler val bar = 2
  @doubler var baz = 3
  @doubler lazy val bax = 4
  @doubler type T = Int
}

@RunWith(classOf[JUnit4])
class Assorted {
  @Test
  def nested: Unit = {
    assertEquals(typeOf[AssortedZoo].decls.sorted.map(_.toString).mkString("\n"), """
      |constructor AssortedZoo
      |method foofoo
      |value barbar
      |value barbar
      |variable bazbaz
      |variable bazbaz
      |variable bazbaz
      |lazy value baxbax
      |type TT
    """.trim.stripMargin)
  }

  @Test
  def local: Unit = {
    @doubler def foo(x: Int) = x
    @doubler val bar = 2
    @doubler var baz = 3
    @doubler lazy val bax = 4
    @doubler type T = Int

    assertEquals(foofoo(1), 1)
    assertEquals(barbar, 2)
    assertEquals(bazbaz, 3)
    assertEquals(baxbax, 4)
    assertEquals(List[TT](5), List(5))
  }
}
