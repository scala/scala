import java.lang.reflect.Modifier._

@SerialVersionUID(42)
class C extends Serializable

@SerialVersionUID(43 - 1)
class D extends Serializable


object Test extends App {
  def checkId(cls: Class[_]): Unit = {
    val field = cls.getDeclaredField("serialVersionUID")
    assert(isPrivate(field.getModifiers))
    field.setAccessible(true)
    val id = field.get(null)
    assert(id == 42, (cls, id))
  }
  checkId(classOf[C])
  checkId(classOf[D])
}
