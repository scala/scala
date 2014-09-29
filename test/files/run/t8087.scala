trait Foo {
  @volatile private[this] var x: String = ""
  @volatile private var y: String = ""
}

class Bar extends Foo

object Test extends App {
  classOf[Bar].getDeclaredFields.foreach(f => {
    assert(java.lang.reflect.Modifier.isVolatile(f.getModifiers), f.getName)
  })
}
