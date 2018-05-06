import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._

@identity case class InteropIdentity(x: Int)
@placebo case class InteropPlacebo(x: Int)

@RunWith(classOf[JUnit4])
class InteropCaseSynthesis {
  @Test
  def caseModuleSynthesisForIdentity: Unit = {
    assertEquals(InteropIdentity.toString, "InteropIdentity")
  }

  @Test
  def caseModuleSynthetisForPlacebo: Unit = {
    assertEquals(InteropPlacebo.toString, "InteropPlacebo")
  }
}