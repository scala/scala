
import scala.util.Properties._

object Test extends App {

  setProp("java.specification.version", "1.7")

  assert( isJavaAtLeast("1.5"))
  assert( isJavaAtLeast("1.6"))
  assert( isJavaAtLeast("1.7"))
  assert(!isJavaAtLeast("1.8"))
  assert(!isJavaAtLeast("1.71"))

  failing(isJavaAtLeast("1.a"))
  failing(isJavaAtLeast("1"))
  failing(isJavaAtLeast(""))
  failing(isJavaAtLeast("."))
  failing(isJavaAtLeast(".5"))
  failing(isJavaAtLeast("1.7.1"))

  def failing(u: =>Unit) = try {
    u
    assert(false, "Expected Exception")
  } catch {
    case _: NumberFormatException =>
  }
}
