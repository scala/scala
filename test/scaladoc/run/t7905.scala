import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
  object A {
    val foo = new B {
      val bar = new C {
        val baz: A.this.type = A.this
      }
    }
  }

  trait B {
    type E = bar.D

    val bar: C
  }

  trait C {
    trait D
  }

  trait G {
    type F = A.foo.E

    def m(f: F) = f match {
      case _: A.foo.bar.D => // error here
    }
  }
  """

  def scaladocSettings = ""

  def testModel(root: Package) = ()
}
