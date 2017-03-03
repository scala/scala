// bytecode should reflect volatile annotation
trait VolatileAbort {
  @volatile private var abortflag = false
}
class DefaultSignalling extends VolatileAbort

object Test {
  def main(args: Array[String]): Unit = {
    val field = classOf[DefaultSignalling].getDeclaredFields.find(_.getName.contains("abortflag")).get
    assert(java.lang.reflect.Modifier.isVolatile(field.getModifiers), field)
  }

}
