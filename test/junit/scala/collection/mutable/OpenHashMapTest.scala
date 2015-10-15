package scala.collection.mutable

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

/** Tests for [[OpenHashMap]]. */
@RunWith(classOf[JUnit4])
class OpenHashMapTest {
  /** Test that an [[OpenHashMap]] correctly maintains its internal `deleted` count. */
  @Test
  def maintainsDeletedCount {
    val m = OpenHashMap.empty[Int, Int]

    // Reflect to get the private `deleted` field's value, which should be zero.

    /* TODO Doesn't work, due to SI-9306.
    import scala.reflect.runtime.{universe => ru}

    val mirror = ru.runtimeMirror(m.getClass.getClassLoader)
    val mapType = ru.typeOf[OpenHashMap[Int, Int]]
    val termSym = mapType.decls
      .filterNot { s => s.isMethod }
      .filter { s => s.fullName.endsWith("deleted") }
      .head.asTerm

    val fieldMirror = mirror.reflect(m).reflectField(termSym)
		*/
    // Use Java reflection instead for now.
    val field = m.getClass.getDeclaredField("scala$collection$mutable$OpenHashMap$$deleted")
    field.setAccessible(true)

    m.put(0, 0)
    m.remove(0)
    assertEquals(1, field.getInt(m))

    m.put(0, 0)  // Add an entry with the same key
    // TODO assertEquals(0, fieldMirror.get.asInstanceOf[Int])
    assertEquals(0, field.getInt(m))
  }
}
