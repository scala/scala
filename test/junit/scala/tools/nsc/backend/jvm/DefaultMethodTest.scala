package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test

import scala.collection.JavaConverters
import scala.collection.JavaConverters._
import scala.reflect.internal.Flags
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.ClassNode
import scala.tools.testing.BytecodeTesting
import scala.tools.testing.BytecodeTesting._

class DefaultMethodTest extends BytecodeTesting {
  import compiler._
  @Test
  def defaultMethodsViaGenBCode(): Unit = {
    import global._
    val code = "package pack { trait T { def foo: Int }}"
    object makeFooDefaultMethod extends Transformer {
      val Foo = TermName("foo")
      /** Transforms a single tree. */
      override def transform(tree: global.Tree): global.Tree = tree match {
        case dd @ DefDef(_, Foo, _, _, _, _) =>
          dd.symbol.setFlag(Flags.JAVA_DEFAULTMETHOD).resetFlag(Flags.DEFERRED)
          copyDefDef(dd)(rhs = Literal(Constant(1)).setType(definitions.IntTpe))
        case _ => super.transform(tree)
      }
    }
    val asmClasses: List[ClassNode] = compiler.compileClassesTransformed(code, Nil, makeFooDefaultMethod.transform(_))
    val foo = asmClasses.head.methods.iterator.asScala.toList.last
    assertTrue("default method should not be abstract", (foo.access & Opcodes.ACC_ABSTRACT) == 0)
    assertTrue("default method body emitted", foo.instructions.size() > 0)
  }
}
