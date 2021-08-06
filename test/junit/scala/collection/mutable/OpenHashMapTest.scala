package scala.collection.mutable

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test
import org.openjdk.jol.info.GraphWalker

/** Tests for [[OpenHashMap]]. */
@deprecated("Tests deprecated API", since="2.13")
class OpenHashMapTest {
  /** Test that an [[OpenHashMap]] correctly maintains its internal `deleted` count. */
  @Test
  def maintainsDeletedCount(): Unit = {
    val m = OpenHashMap.empty[Int, Int]

    // Reflect to get the private `deleted` field's value, which should be zero.
    // Was broken, see scala/bug#9306.
    import scala.reflect.runtime.{universe => ru}

    val mirror = ru.runtimeMirror(m.getClass.getClassLoader)
    val mapType = ru.typeOf[OpenHashMap[Int, Int]]
    val termSym = mapType.decls
      .filterNot { s => s.isMethod }
      .filter { s => s.fullName.endsWith("deleted") }
      .head.asTerm

    val fieldMirror = mirror.reflect(m).reflectField(termSym)

    // Use Java reflection instead for now.
    val field =
      try {  // Name may or not be mangled, depending on what the compiler authors are doing.
        m.getClass.getDeclaredField("scala$collection$mutable$OpenHashMap$$deleted")
      } catch {
        case _: NoSuchFieldException =>
          m.getClass.getDeclaredField("deleted")
      }
    field.setAccessible(true)

    m.put(0, 0)
    m.remove(0)
    assertEquals(1, field.getInt(m))

    m.put(0, 0)  // Add an entry with the same key
    assertEquals(0, fieldMirror.get.asInstanceOf[Int])
    assertEquals(0, field.getInt(m))
  }

  /** Test that an [[OpenHashMap]] frees references to a deleted key (scala/bug#9522). */
  @Test
  def freesDeletedKey(): Unit = {

    class MyClass {
      override def hashCode() = 42
    }

    val walker = new GraphWalker()
    def countInstances(obj: AnyRef) = walker.walk(obj).getClassCounts().count(classOf[MyClass]: Class[_])

    val m = OpenHashMap.empty[MyClass, Int]
    val obj = new MyClass
    assertEquals(0, countInstances(m), "Found a key instance in the map before adding one!?")
    m.put(obj, 0)
    assertEquals(1, countInstances(m), "There should be only one key instance in the map.")
    m.put(obj, 1)
    assertEquals(1, countInstances(m), "There should still be only one key instance in the map.")
    m.remove(obj)
    assertEquals(0, countInstances(m), "There should be no key instance in the map.")

    val obj2 = new MyClass
    assertEquals(obj.##, obj2.##, "The hash codes of the test objects need to match.")
    m.put(obj, 0)
    m.put(obj2, 0)
    assertEquals(2, countInstances(m), "There should be two key instances in the map.")
    m.remove(obj)
    assertEquals(1, countInstances(m), "There should be one key instance in the map.")
    m.remove(obj2)
    assertEquals(0, countInstances(m), "There should be no key instance in the map.")
  }
}
