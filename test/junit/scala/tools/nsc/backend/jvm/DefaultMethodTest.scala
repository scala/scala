package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test

import scala.collection.JavaConverters
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.ClassNode
import scala.tools.nsc.backend.jvm.CodeGenTools._
import JavaConverters._
import scala.tools.testing.ClearAfterClass

object DefaultMethodTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler(extraArgs = "-Ybackend:GenBCode")
  def clear(): Unit = { compiler = null }
}

class DefaultMethodTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = DirectCompileTest
  val compiler = DirectCompileTest.compiler

  @Test
  def defaultMethodsViaGenBCode(): Unit = {
    import compiler._
    val code = "package pack { trait T { def foo: Int }}"
    object makeFooDefaultMethod extends Transformer {
      val Foo = TermName("foo")
      /** Transforms a single tree. */
      override def transform(tree: compiler.Tree): compiler.Tree = tree match {
        case dd @ DefDef(_, Foo, _, _, _, _) =>
          dd.symbol.setFlag(reflect.internal.Flags.JAVA_DEFAULTMETHOD)
          copyDefDef(dd)(rhs = Literal(Constant(1)).setType(definitions.IntTpe))
        case _ => super.transform(tree)
      }
    }
    val asmClasses: List[ClassNode] = readAsmClasses(compileTransformed(compiler)(code, Nil, makeFooDefaultMethod.transform(_)))
    val foo = asmClasses.head.methods.iterator.asScala.toList.last
    assertTrue("default method should not be abstract", (foo.access & Opcodes.ACC_ABSTRACT) == 0)
    assertTrue("default method body emitted", foo.instructions.size() > 0)
  }


}
