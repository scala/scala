import scala.tools.nsc._
import io.{ AbstractFile }
import util.{ SourceFile, BatchSourceFile, stringFromStream }
import scala.tools.nsc.reporters.ConsoleReporter

object Test {
  val src: SourceFile = new BatchSourceFile("src", """
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
  """)

  def mkCompiler(args: String*) = {
    val settings             = new Settings()
    val command              = new CompilerCommand("-usejavacp" :: args.toList, settings)

    new Global(settings)
  }
  
  def slurp(body: => Unit): String = stringFromStream { stream => 
    Console.withOut(stream) {
      Console.withErr(stream) {
        body
      }
    }
  }
  def lines(args: String*): List[String] = {
    val output = slurp {
      val compiler = mkCompiler(args: _*)
      val run = new compiler.Run()
      run.compileSources(List(src))
    }
    output split "\\n" toList
  }
  def showClass(name: String) = lines("-Yshow:typer", "-Xshow-class", name)
  def showObject(name: String) = lines("-Yshow:typer", "-Xshow-object", name)
  
  def show(xs: List[String]) = {
    xs filter (x => (x contains "def showdefTestMember") || (x startsWith "<<-- ")) foreach println
  }
  
  def main(args: Array[String]) {
    show(List("Bippy", "Bippy#BippyType", "Bippy.BippyType", "Bippy#Boppity", "Bippy#Boppity#Boo") flatMap showClass)
    show(List("Bippy", "Bippy#Boppity#Boo") flatMap showObject)
  }
}
