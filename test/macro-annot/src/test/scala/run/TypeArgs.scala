import org.junit._
import org.junit.runner._
import org.junit.runners._
import Assert._

@RunWith(classOf[JUnit4])
class TypeArgs {
  @Test
  def macroAnnotationsWithTypeArgsExpand: Unit = {
    @shove[Int] val description = "I’m an Int!"
    @shove[String] val bar = "I’m a String!"
    assertEquals(5.description, "I’m an Int!")
    assertEquals("foo".bar, "I’m a String!")
  }
}