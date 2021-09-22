package scala.tools
package testing

import java.lang.ref._
import java.lang.reflect.{Field, Modifier}
import java.util.IdentityHashMap

import org.junit.Assert._

import scala.collection.{GenIterable, IterableLike, mutable}
import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime.stringOf
import scala.util.control.{ControlThrowable, NonFatal}

/** This module contains additional higher-level assert statements
 *  that are ultimately based on junit.Assert primitives.
 */
object AssertUtil {
  // junit fail is Unit
  def fail(message: String): Nothing = throw new AssertionError(message)

  private final val timeout = 60 * 1000L // wait a minute

  private implicit class `ref helper`[A](val r: Reference[A]) extends AnyVal {
    def isEmpty: Boolean = r.get == null
    def nonEmpty: Boolean = !isEmpty
  }

  private implicit class `class helper`(val clazz: Class[_]) extends AnyVal {
    def allFields: List[Field] = {
      def loop(k: Class[_]): List[Field] =
        if (k == null) Nil
        else k.getDeclaredFields.toList ::: loop(k.getSuperclass)

      loop(clazz)
    }
  }

  private implicit class `field helper`(val f: Field) extends AnyVal {
    def follow(o: AnyRef): AnyRef = {
      f setAccessible true
      f get o
    }
  }

  /** Check that throwable T (or a subclass) was thrown during evaluation of `body`,
   * and that its message satisfies the `checkMessage` predicate.
   * Any other exception is propagated.
   */
  def assertThrows[T <: Throwable : ClassTag](body: => Any,
                                              checkMessage: String => Boolean = _ => true): Unit = {
    assertThrown[T](t => checkMessage(t.getMessage))(body)
  }

  private val Unthrown = new ControlThrowable {}

  def assertThrown[T <: Throwable : ClassTag](checker: T => Boolean)(body: => Any): Unit =
    try {
      body
      throw Unthrown
    } catch {
      case Unthrown           => fail("Expression did not throw!")
      case e: T if checker(e) => ()
      case failed: T          =>
        val ae = new AssertionError(s"Exception failed check: $failed")
        ae.addSuppressed(failed)
        throw ae
      case NonFatal(other)    =>
        val ae = new AssertionError(s"Wrong exception: expected ${implicitly[ClassTag[T]]} but was ${other.getClass.getName}")
        ae.addSuppressed(other)
        throw ae
    }

  /** JUnit-style assertion for `IterableLike.sameElements`.
   */
  def assertSameElements[A, B >: A](expected: IterableLike[A, _], actual: GenIterable[B], message: String = ""): Unit =
    if (!expected.sameElements(actual))
      fail(
        f"${if (message.nonEmpty) s"$message " else ""}expected:<${stringOf(expected)}> but was:<${stringOf(actual)}>"
        )

  /** Convenient for testing iterators.
   */
  def assertSameElements[A, B >: A](expected: IterableLike[A, _], actual: Iterator[B]): Unit =
    assertSameElements(expected, actual.toList, "")

  /** Value is not strongly reachable from roots after body is evaluated.
   */
  def assertNotReachable[A <: AnyRef](a: => A, roots: AnyRef*)(body: => Unit): Unit = {
    val wkref = new WeakReference(a)
    // fail if following strong references from root discovers referent. Quit if ref is empty.
    def assertNoRef(root: AnyRef): Unit = {
      val seen  = new IdentityHashMap[AnyRef, Unit]
      val stack = new mutable.Stack[AnyRef]()
      def loop(): Unit = if (wkref.nonEmpty && stack.nonEmpty) {
        val o: AnyRef = stack.pop()
        if (o != null && !seen.containsKey(o)) {
          seen.put(o, ())
          assertFalse(s"Root $root held reference $o", o eq wkref.get)
          o match {
            case a: Array[AnyRef] =>
              a.foreach(e => if (!e.isInstanceOf[Reference[_]]) stack.push(e))
            case _ =>
              for (f <- o.getClass.allFields)
                if (!Modifier.isStatic(f.getModifiers) && !f.getType.isPrimitive && !classOf[Reference[_]].isAssignableFrom(f.getType))
                  stack.push(f.follow(o))
          }
        }
        loop()
      }
      stack.push(root)
      loop()
    }
    body
    roots.foreach(assertNoRef)
  }
}
