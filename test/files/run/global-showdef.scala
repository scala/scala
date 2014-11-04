import scala.tools.partest.DirectTest
import scala.tools.nsc.util.stringFromStream

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Yshow:typer -Ystop-after:typer"

  override def code = """
package foo.bar

class Bippy {
  type BippyType <: {
    def showdefTestMemberType1: Unit
  }

  def showdefTestMemberClass1 = 5
  class Boppity {
    def showdefTestMemberClass2 = 5
    class Boo {
      def showdefTestMemberClass3 = 5
    }
    object Boo {
      def showdefTestMemberObject1 = "abc"
    }
  }
}

object Bippy {
  type BippyType <: {
    def showdefTestMemberType2: Unit
  }

  def showdefTestMemberObject2 = "abc"
}
  """

  override def show(): Unit = {
    val classes = List("Bippy", "Bippy#BippyType", "Bippy.BippyType", "Bippy#Boppity", "Bippy#Boppity#Boo")
    val objects = List("Bippy", "Bippy#Boppity#Boo")

    def interesting(line: String) = (line contains "def showdefTestMember") || (line startsWith "<<-- ")

    def run(args: String*) = slurp(args: _*).lines filter interesting foreach println

    classes.zipAll(objects, "", "") foreach {
      case (c, "") => run("-Xshow-class", c)
      case (c, o)  => run("-Xshow-class", c, "-Xshow-object", o)
    }
  }

  // slurp the compilation result
  def slurp(args: String*): String = stringFromStream { stream =>
    Console.withOut(stream) {
      Console.withErr(stream) {
        compile(args: _*)
      }
    }
  }
}
