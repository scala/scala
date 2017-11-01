package p1

class ReflectTest {
  def test(a: AnyRef): Unit = {
    val mirror = reflect.runtime.universe.runtimeMirror(a.getClass.getClassLoader)
    println(mirror.reflect(a).symbol)
  }
}
