package scala.collection.immutable
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class MapTest {

  // Test if compile, see https://github.com/scala/bug/issues/11314
  @Test
  def test(): Unit = {
    var m = Map("foo" -> "bar").withDefaultValue("baz")
    m += "a" -> "b"
  }
}
