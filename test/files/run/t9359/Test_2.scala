import scala.tools.partest.BytecodeTest
import scala.tools.asm
import asm.tree.{ClassNode, InnerClassNode}
import asm.{Opcodes => Flags}
import scala.collection.JavaConverters._

class C {
  def f1: A_1.A1N_FINAL    = A_1.A1N_FINAL.A1N_FINAL_VAL
  def f2: A_1.A1N          = A_1.A1N.A1N_VAL
  def f3: A_1.A1N_ABSTRACT = A_1.A1N_ABSTRACT.A1N_ABSTRACT_VAL

  def f4: B_2.A1N_FINAL    = B_2.A1N_FINAL.A1N_FINAL_VAL
  def f5: B_2.A1N          = B_2.A1N.A1N_VAL
  def f6: B_2.A1N_ABSTRACT = B_2.A1N_ABSTRACT.A1N_ABSTRACT_VAL
}

object Test extends BytecodeTest {
  def tost(n: InnerClassNode) = {
    val t = new asm.util.Textifier
    t.visitInnerClass(n.name, n.outerName, n.innerName, n.access)
    t.getText.get(0);
  }
  def show(): Unit = {
    for (n <- loadClassNode("C").innerClasses.asScala.toList.sortBy(_.name)) {
      println(tost(n))
    }
  }
}
