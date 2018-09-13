package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.NameTransformer
import scala.tools.asm.Opcodes
import scala.tools.testing.BytecodeTesting

@RunWith(classOf[JUnit4])
class ModuleFieldTest extends BytecodeTesting {
  import compiler._

  @Test
  def moduleFieldIsFinalForPureModules(): Unit = {
    val code =
      """
        |object Pure1
        |case class Pure2(x: Int) // companion extends FunctionN, but we know that's pure
        |object Impure1 { println("") }
        |class UnknownPurity
        |object Impure2 extends UnknownPurity
      """.stripMargin
    val classes = compileClasses(code)

    def check(name: String, expected: Boolean): Unit = {
      val moduleField = classes.find(_.name == name).get.fields.get(0)
      assert(moduleField.name == NameTransformer.MODULE_INSTANCE_NAME)
      val isFinal = (moduleField.access & Opcodes.ACC_FINAL) != 0
      assertEquals(expected, isFinal)
    }
    check("Pure1$", true)
    check("Pure2$", true)
    check("Impure1$", true)
    check("Impure2$", true)
  }
}
