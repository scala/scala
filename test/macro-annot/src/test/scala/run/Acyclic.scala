import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._

@RunWith(classOf[JUnit4])
class Acyclic {
  @Test
  def testA: Unit = {
    import acyclica._
    assertEquals(C.toString, "C")
    assertEquals(D.toString, "D")
    assertEquals(new CX().toString, "CX")
    assertEquals(new DX().toString, "DX")
  }

  @Test
  def testB: Unit = {
    import acyclicb._
    assertEquals(CC.x.toString, "DX")
    assertEquals(DD.x.toString, "CX")
  }

  @Test
  def testC: Unit = {
    import Module4._
    @identity4 class C4
  }
}