package scala.tools.nsc.backend.bcode

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.asm
import scala.collection.JavaConverters._

import scala.tools.asm.Opcodes

@RunWith(classOf[JUnit4])
class LabelsCleanupUnit1Test {

  @Test
  def show: Unit = {
    val ma  = transformed(before())
    val mb  = after()
    val isa = wrapped(ma)
    val isb = wrapped(mb)
    // redundant LineNumberNodes are removed
    assert(isa == isb)
  }

  def wrapped(m: asm.tree.MethodNode) = {
    Util.computeMaxLocalsMaxStack(m)
    Util.textify(m)
  }

  def mkMethodNode = {
    new asm.tree.MethodNode(
      Opcodes.ACC_PUBLIC,
      "m",
      "()V",
      null, null
    )
  }

  def before(): asm.tree.MethodNode = {
    val m  = mkMethodNode
    val L = new asm.Label
    //     return
    m.visitInsn(Opcodes.RETURN)
    // line number 1
    m.visitLineNumber(1, L)
    // line number 2
    m.visitLineNumber(2, L)
    // L:
    m.visitLabel(L)

    m
  }

  def after(): asm.tree.MethodNode = {
    val m  = mkMethodNode
    //     return
    m.visitInsn(Opcodes.RETURN)

    m
  }

  def transformed(input: asm.tree.MethodNode): asm.tree.MethodNode = {
    val tr = new LabelsCleanup
    do { tr.transform(input) } while (tr.changed)

    input
  }

}
