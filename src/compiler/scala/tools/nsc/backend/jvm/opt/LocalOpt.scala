/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.tools.asm.{MethodWriter, ClassWriter}
import scala.tools.asm.tree.analysis.{Analyzer, BasicValue, BasicInterpreter}
import scala.tools.asm.tree.{LabelNode, ClassNode, MethodNode}
import scala.collection.convert.decorateAsScala._

/**
 * Intra-Method optimizations.
 */
object LocalOpt {
  /**
   * Remove unreachable instructions from all (non-abstract) methods.
   *
   * @param clazz The class whose methods are optimized
   * @return      `true` if unreachable code was elminated in some method, `false` otherwise.
   */
  def removeUnreachableCode(clazz: ClassNode): Boolean = {
    clazz.methods.asScala.foldLeft(false) {
      case (changed, method) => removeUnreachableCode(method, clazz) || changed
    }
  }

  /**
   * Remove unreachable code from a method.
   * We rely on dead code elimination provided by the ASM framework, as described in the ASM User
   * Guide (http://asm.ow2.org/index.html), Section 8.2.1. It runs a data flow analysis, which only
   * computes Frame information for reachable instructions. Instructions for which no Frame data is
   * available after the analyis are unreachable.
   */
  private def removeUnreachableCode(method: MethodNode, ownerClass: ClassNode): Boolean = {
    val initialSize = method.instructions.size
    if (initialSize == 0) return false

    // The data flow analysis requires the maxLocals / maxStack fields of the method to be computed.
    computeMaxLocalsMaxStack(method)
    val a = new Analyzer[BasicValue](new BasicInterpreter)
    a.analyze(ownerClass.name, method)
    val frames = a.getFrames

    var i = 0
    val itr = method.instructions.iterator()
    while (itr.hasNext) {
      val ins = itr.next()
      // Don't remove label nodes: they might be referenced for example in a LocalVariableNode
      if (frames(i) == null && !ins.isInstanceOf[LabelNode]) {
        // Instruction iterators allow removing during iteration.
        // Removing is O(1): instructions are doubly linked list elements.
        itr.remove()
      }
      i += 1
    }

    method.instructions.size == initialSize
  }

  /**
   * In order to run an Analyzer, the maxLocals / maxStack fields need to be available. The ASM
   * framework only computes these values during bytecode generation.
   *
   * Sicne there's currently no better way, we run a bytecode generator on the method and extract
   * the computed values. This required changes to the ASM codebase:
   *   - the [[MethodWriter]] class was made public
   *   - accessors for maxLocals / maxStack were added to the MethodWriter class
   *
   * We could probably make this faster (and allocate less memory) by hacking the ASM framework
   * more: create a subclass of MethodWriter with a /dev/null byteVector. Another option would be
   * to create a separate visitor for computing those values, duplicating the functionality from the
   * MethodWriter.
   */
  private def computeMaxLocalsMaxStack(method: MethodNode) {
    val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    val excs = method.exceptions.asScala.toArray
    val mw = cw.visitMethod(method.access, method.name, method.desc, method.signature, excs).asInstanceOf[MethodWriter]
    method.accept(mw)
    method.maxLocals = mw.getMaxLocals
    method.maxStack = mw.getMaxStack
  }
}
