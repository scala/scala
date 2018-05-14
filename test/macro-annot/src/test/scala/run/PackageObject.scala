import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._

package object pkg1 {
  @doubler def foo(x: Int) = x
  @doubler val bar = 2
  @doubler var baz = 3
  @doubler lazy val bax = 4
  @doubler type T = Int
}

package pkg2 {
  @hello object `package`
}

@RunWith(classOf[JUnit4])
class PackageObject {
  @Test
  def packageObjectMembers: Unit = {
    import pkg1._
    assertEquals(foofoo(1), 1)
    assertEquals(barbar, 2)
    assertEquals(bazbaz, 3)
    assertEquals(baxbax, 4)
    assertEquals(List[TT](5), List(5))
  }

  @Test
  def packageObjectItself: Unit = {
    assertEquals(pkg2.hello, "hello")
  }
}
