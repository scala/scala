package scala.tools.nsc.backend.bcode

import scala.tools.asm

object BytecodeAssert {

  def assertEquals(expected: asm.tree.MethodNode, actual: asm.tree.MethodNode): Unit = {
    val wrappedExpected = wrapped(expected)
    val wrappedActual   = wrapped(actual)
    org.junit.Assert.assertEquals(wrappedExpected, wrappedActual)
  }

  def assertNotEquals(expected: asm.tree.MethodNode, actual: asm.tree.MethodNode): Unit = {
    val wrappedExpected = wrapped(expected)
    val wrappedActual   = wrapped(actual)
    org.junit.Assert.assertNotEquals(wrappedExpected, wrappedActual)
  }

  private def wrapped(m: asm.tree.MethodNode): String = {
    Util.computeMaxLocalsMaxStack(m)
    Util.textify(m)
  }

}
