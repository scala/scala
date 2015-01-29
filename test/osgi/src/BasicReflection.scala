package tools.test.osgi
package reflection
package basic

import scala.language.higherKinds

import org.junit.Assert._
import org.ops4j.pax.exam.CoreOptions._

import org.junit.Test
import org.junit.runner.RunWith
import org.ops4j.pax.exam
import org.ops4j.pax.exam.Configuration
import org.ops4j.pax.exam.junit.PaxExam
import org.ops4j.pax.exam.spi.reactors.{ ExamReactorStrategy, PerMethod }
import org.ops4j.pax.swissbox.tracker.ServiceLookup
import org.osgi.framework.BundleContext


class C {
  val f1 = 2
  var f2 = 3

  def m1 = 4
  def m2() = 5
  def m3[T >: String <: Int]: T = ???
  def m4[A[_], B <: A[Int]](x: A[B])(implicit y: Int) = ???
  def m5(x: => Int, y: Int*): String = ???

  class C
  object M

  override def toString = "an instance of C"
}
object M


@RunWith(classOf[PaxExam])
@ExamReactorStrategy(Array(classOf[PerMethod]))
class BasicReflectionTest extends ScalaOsgiHelper {

  @Configuration
  def config(): Array[exam.Option] =
    justReflectionOptions

  // Ensure Pax-exam requires C/M in our module
  def dummy = {
    new C
    M.toString
  }

  @Test
  def basicMirrorThroughOsgi(): Unit = {
    // Note for now just assert that we can do this stuff.
    import scala.reflect.runtime.universe._
    val cm = runtimeMirror(classOf[C].getClassLoader)
    val im = cm.reflect(new C)
    assertEquals("Unable to reflect field name!",
                 "value f1",
                 im.reflectField(typeOf[C].member(TermName("f1")).asTerm).symbol.toString)
    assertEquals("Unable to reflect value!",
                 2,
                 im.reflectField(typeOf[C].member(TermName("f1")).asTerm).get)
 }
}
