//> using options -Xlint:serial
//
@SerialVersionUID(0L) // should have no effect
trait T

object Test extends App {
  try {
    classOf[T].getDeclaredField("serialVersionUID")
    assert(false)
  } catch {
    case nsfe: NoSuchFieldException =>
  }
}
