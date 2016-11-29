package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.openjdk.jol.info.{GraphPathRecord, GraphVisitor, GraphWalker}

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
    // TODO assertEquals(0, fieldMirror.get.asInstanceOf[Int])
    assertEquals(0, field.getInt(m))
  }

  /** Test that an [[OpenHashMap]] frees references to a deleted key (SI-9522). */
  @Test
  def freesDeletedKey {
    import scala.language.reflectiveCalls

    class MyClass {
      override def hashCode() = 42
    }

    val counter = new GraphVisitor() {
      private[this] var instanceCount: Int = _

      def countInstances(obj: AnyRef) = {
        instanceCount = 0
        val walker = new GraphWalker(obj)
        walker.addVisitor(this)
        walker.walk
        instanceCount
      }

      override def visit(record: GraphPathRecord) {
        if (record.klass() == classOf[MyClass])  instanceCount += 1
      }
    }

    val m = OpenHashMap.empty[MyClass, Int]
    val obj = new MyClass
    assertEquals("Found a key instance in the map before adding one!?", 0, counter.countInstances(m))
    m.put(obj, 0)
    assertEquals("There should be only one key instance in the map.", 1, counter.countInstances(m))
    m.put(obj, 1)
    assertEquals("There should still be only one key instance in the map.", 1, counter.countInstances(m))
    m.remove(obj)
    assertEquals("There should be no key instance in the map.", 0, counter.countInstances(m))

    val obj2 = new MyClass
    assertEquals("The hash codes of the test objects need to match.", obj.##, obj2.##)
    m.put(obj, 0)
    m.put(obj2, 0)
    assertEquals("There should be two key instances in the map.", 2, counter.countInstances(m))
    m.remove(obj)
    assertEquals("There should be one key instance in the map.", 1, counter.countInstances(m))
    m.remove(obj2)
    assertEquals("There should be no key instance in the map.", 0, counter.countInstances(m))
  }
}
