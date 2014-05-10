import annotation._

@SerialVersionUID(42) @strictfp class Foo[@specialized(Int) T] extends Serializable {
  def foo(t: T) = t
}

object Test extends App {
  def checkUID(cls: Class[_], expected: Long) = {
    val actual = java.io.ObjectStreamClass.lookup(cls).getSerialVersionUID
    assert(actual == expected, s"$actual != expected for ${cls}")
  }
  def checkStrictFp(cls: Class[_]) = {
    import java.lang.reflect._
    for (m <- cls.getDeclaredMethods) {
      val isStrict = Modifier.isStrict(m.getModifiers)
      assert(isStrict, cls)
    }
  }
  def check(x: AnyRef) {
    checkUID(x.getClass, 42)
    checkStrictFp(x.getClass)
  }

  check(new Foo[String])
  check(new Foo[Int])
}

