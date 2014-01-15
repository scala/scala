/* Check on the processing of annotated types.  Initially this tested
 * asSeenFrom, but now it also tests packedType and the treatment
 * of DeBruijn's .  It runs the test using the interpreter so that
 * the resulting annotated types can be printed out.
 */
import scala.tools.nsc.Settings
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """

class Annot(obj: Any) extends annotation.Annotation with annotation.TypeConstraint

class A {
  val x = "hello"
  val y: Int @Annot(x) = 10
  override def toString = "an A"
}

val a = new A
val y = a.y   // should rewrite "this.x" to "a.x"
var a2 = new A
val y2 = a2.y   // should drop the annotation

object Stuff {
  val x = "hello"
  val y : Int @Annot(x) = 10
}

val y = Stuff.y // should rewrite the annotation

class B {
  val y: Int @Annot(Stuff.x) = 10
  override def toString = "a B"
}

val b = new B
val y = b.y  // should keep the annotation
def m(x: String): String @Annot(x) = x

val three = "three"
val three2 = m(three:three.type)  // should change x to three
var four = "four"
val four2 = m(four) // should have an existential bound
val four3 = four2   // should have the same type as four2
val stuff = m("stuff") // should not crash

class peer extends annotation.Annotation // should not crash

class NPE[T <: NPE[T] @peer] // should not crash

def m = {
  val x = "three"
  val y : String @Annot(x) = x
  y
} // x should not escape the local scope with a narrow type

def n(y: String) = {
  def m(x: String) : String @Annot(x) = {
    (if (x == "")
      m("default")
    else
      x)
  }
  m("stuff".stripMargin)
} // x should be existentially bound

class rep extends annotation.Annotation { }

object A { val x = "hello" : String @ rep }

val y = a.x // should drop the annotation

val x = 3 : Int @Annot(e+f+g+h) // should have a graceful error message
"""

  override def transformSettings(s: Settings): Settings = {
    s.Xexperimental.value = true
    s.deprecation.value = true
    // when running that compiler, give it a scala-library to the classpath
    s.classpath.value = sys.props("java.class.path")
    s
  }
}
