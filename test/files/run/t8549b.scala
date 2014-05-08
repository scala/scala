
@SerialVersionUID(42)
class C

@SerialVersionUID(43 - 1)
class D


object Test extends App {
  def checkId(cls: Class[_]) {
    val id = cls.getDeclaredField("serialVersionUID").get(null)
    assert(id == 42, (cls, id))  
  }
  checkId(classOf[C])
  checkId(classOf[D])
}
