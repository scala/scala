import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._

@RunWith(classOf[JUnit4])
class Argumentative {
  @Test
  def combo: Unit = {
    @argumentative(1, 2) object X
    assertEquals(X.toString, "1 2")
  }
}