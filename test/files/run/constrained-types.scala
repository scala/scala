/* Check on the processing of annotated types.  Initially this tested
 * asSeenFrom, but now it also tests packedType and the treatment
 * of DeBruijn's .  It runs the test using the interpreter so that
 * the resulting annotated types can be printed out.
 */
import scala.tools.nsc._
import java.io._
import scala.Console

object Test {

  val testCode = List(
    "class Annot(obj: Any) extends Annotation with TypeConstraint",

    """class A {
      |  val x = "hello"
      |  val y: Int @Annot(x) = 10
      |  override def toString = "an A"
      |} """,



    "val a = new A",
    
    """val y = a.y   // should rewrite "this.x" to "a.x" """,


    "var a2 = new A",
    "val y2 = a2.y   // should drop the annotation",


    """object Stuff {
      |  val x = "hello"
      |  val y : Int @Annot(x) = 10
      |}""",

    "val y = Stuff.y // should rewrite the annotation",

    """class B {
      |  val y: Int @Annot(Stuff.x) = 10
      |  override def toString = "a B"
      |}""",

    "val b = new B",
    "val y = b.y  // should keep the annotation",


    "def m(x: String): String @Annot(x) = x",
    "val three = \"three\"",
    "val three2 = m(three:three.type)  // should change x to three",
    "var four = \"four\"",
    "val four2 = m(four) // should have an existential bound",
    "val four3 = four2   // should have the same type as four2",

    """val stuff = m("stuff") // should not crash""",

    """class peer extends Annotation // should not crash""", // reported by Manfred Stock
    """class NPE[T <: NPE[T] @peer] // should not crash""",  // reported by Manfred Stock

    """def m = {
      |  val x = "three"
      |  val y : String @Annot(x) = x
      |  y
      |} // x should be existentially bound""",

    """def n(y: String) = {
      |  def m(x: String) : String @Annot(x) = {
      |    (if (x == "")
      |      m("default")
      |    else
      |      x)
      |  }
      |  m("stuff".stripMargin)
      |} // x should be existentially bound""",

    "class rep extends Annotation",
    """object A { val x = "hello" : String @ rep }""",
    "val y = a.x // should drop the annotation",

    "val x = 3 : Int @Annot(e+f+g+h) //should have a graceful error message",  

    "class Where(condition: Boolean) extends Annotation",
    "val x : Int @Where(self > 0 && self < 100) = 3"
    ).map(_.stripMargin)



  def main(args: Array[String]) {
    val settings = new Settings
    settings.Xexperimental.value = true
    settings.selfInAnnots.value = true
    settings.deprecation.value = true
    // when running that compiler, give it a scala-library to the classpath
    settings.classpath.value = System.getProperty("java.class.path")

    val interp = new Interpreter(settings)

    for (cmd <- testCode) {
      println(cmd)
      interp.interpret(cmd)
      println()
      println("-----")
    }
  }
}
