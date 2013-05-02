/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package jvm

import scala.tools.asm
import asm.Opcodes
import asm.optimiz.Util
import asm.tree._

import scala.collection.{ mutable, immutable }
import collection.convert.Wrappers.JListWrapper

/*
 *  Optimize and tidy-up bytecode before it's serialized for good.
 *  This class focuses on
 *    - intra-method optimizations,
 *    - intra-class  optimizations, and
 *    - utilities for the above and for inter-procedural optimizations as well.
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 *  TODO Improving the Precision and Correctness of Exception Analysis in Soot, http://www.sable.mcgill.ca/publications/techreports/#report2003-3
 *
 */
abstract class BCodeOptIntra extends BCodeTypes {

  import global._

  /*
   *  All methods in this class can-multi-thread
   */
  class EssentialCleanser(cnode: asm.tree.ClassNode) {

    val jumpsCollapser      = new asm.optimiz.JumpChainsCollapser()
    val labelsCleanup       = new asm.optimiz.LabelsCleanup()
    val danglingExcHandlers = new asm.optimiz.DanglingExcHandlers()

    /*
     *  This method performs a few intra-method optimizations:
     *    - collapse a multi-jump chain to target its final destination via a single jump
     *    - remove unreachable code
     *    - remove those LabelNodes and LineNumbers that aren't in use
     *
     *  Some of the above are applied repeatedly until no further reductions occur.
     *
     *  Node: what ICode calls reaching-defs is available as asm.tree.analysis.SourceInterpreter, but isn't used here.
     *
     */
    final def cleanseMethod(cName: String, mnode: asm.tree.MethodNode): Boolean = {

      var changed = false
      var keepGoing = false

      do {
        keepGoing = false

        jumpsCollapser.transform(mnode)            // collapse a multi-jump chain to target its final destination via a single jump
        keepGoing |= jumpsCollapser.changed

        keepGoing |= removeUnreachableCode(mnode)

        labelsCleanup.transform(mnode)             // remove those LabelNodes and LineNumbers that aren't in use
        keepGoing |= labelsCleanup.changed

        danglingExcHandlers.transform(mnode)
        keepGoing |= danglingExcHandlers.changed

        changed |= keepGoing

      } while (keepGoing)

      changed

    }

    /*
     * Detects and removes unreachable code.
     *
     * Should be used last in a transformation chain, before stack map frames are computed.
     * The Java 6 verifier demands frames be available even for dead code.
     * Those frames are tricky to compute, http://asm.ow2.org/doc/developer-guide.html#deadcode
     * The problem is avoided altogether by not emitting unreachable code in the first place.
     *
     * This method has a lower memory footprint than `asm.optimiz.UnreachableCode`
     * Otherwise both versions accomplish the same.
     *
     */
    final def removeUnreachableCode(mnode: MethodNode): Boolean = {

      val landing  = mutable.Set.empty[AbstractInsnNode]
      val suspect  = mutable.Set.empty[AbstractInsnNode]
      val worklist = new mutable.Stack[AbstractInsnNode]

          def transfer(to: AbstractInsnNode) {
            if (to == null)  { return }
            suspect -= to
            if (landing(to)) { return }
            landing += to
            if (to.getType == AbstractInsnNode.LABEL) { transfer(to.getNext) }
            else {
              worklist push to
            }
          }

          def transfers(labels: _root_.java.util.List[LabelNode]) {
            for(lbl <- JListWrapper(labels)) { transfer(lbl) }
          }

          def makeSuspect(s: AbstractInsnNode) {
            if (s == null) { return }
            if (!landing(s)) {
              suspect += s
             }
          }

      val stream = mnode.instructions
      transfer(stream.getFirst)
      for(tcb <- JListWrapper(mnode.tryCatchBlocks)) { transfer(tcb.handler) }

      while (worklist.nonEmpty) {
        var reach = worklist.pop()
        while (reach != null) {

          reach.getType match {
            case AbstractInsnNode.LABEL =>
              transfer(reach)
              reach = null
            case AbstractInsnNode.JUMP_INSN =>
              val ji = reach.asInstanceOf[JumpInsnNode]
              if (ji.getOpcode == Opcodes.JSR) {
                return false // don't touch methods containing subroutines (perhaps was inlined, scalac doesn't emit JSR/RET)
              }
              if (Util.isCondJump(reach)) {
                transfer(ji.label)
                transfer(reach.getNext)
              } else {
                assert(reach.getOpcode == Opcodes.GOTO)
                transfer(ji.label)
                makeSuspect(reach.getNext)
              }
              reach = null
            case AbstractInsnNode.LOOKUPSWITCH_INSN =>
              val lsi = reach.asInstanceOf[LookupSwitchInsnNode]
              transfer(lsi.dflt)
              transfers(lsi.labels)
              reach = null
            case AbstractInsnNode.TABLESWITCH_INSN =>
              val tsi = reach.asInstanceOf[TableSwitchInsnNode]
              transfer(tsi.dflt)
              transfers(tsi.labels)
              reach = null
            case AbstractInsnNode.INSN =>
              val isATHROW = (reach.getOpcode == Opcodes.ATHROW)
              if (isATHROW || Util.isRETURN(reach)) {
                makeSuspect(reach.getNext)
                reach = null
              }
            case _ =>
              if (reach.getOpcode == Opcodes.RET) {
                return false // don't touch methods containing subroutines (perhaps was inlined, scalac doesn't emit JSR/RET)
              }
              ()
          }

          if (reach != null) {
            reach = reach.getNext
          }

        }
      }

      // pruning
      var changed = false
      for(s <- suspect) {
        var current = s
        while (current != null && !landing(current) && stream.contains(current)) {
          val nxt = current.getNext
          if (current.getType != AbstractInsnNode.LABEL) { // let asm.optimiz.LabelsCleanup take care of LabelNodes
            changed = true
            stream remove current
          }
          current = nxt
        }
      }

      changed
    }

    /*
     *  Removes dead code.
     *
     *  When writing classfiles with "optimization level zero" (ie -neo:GenBCode)
     *  the very least we want to do is remove dead code beforehand,
     *  so as to prevent an artifact of stack-frames computation from showing up,
     *  the artifact described at http://asm.ow2.org/doc/developer-guide.html#deadcode
     *  That artifact results from the requirement by the Java 6 split verifier
     *  that a stack map frame be available for each basic block, even unreachable ones.
     *
     *  Just removing dead code might leave stale LocalVariableTable entries
     *  thus `cleanseMethod()` also gets rid of those.
     *
     */
    final def codeFixupDCE() {
      val iter = cnode.methods.iterator()
      while (iter.hasNext) {
        val mnode = iter.next()
        if (Util.hasBytecodeInstructions(mnode)) {
          Util.computeMaxLocalsMaxStack(mnode)
          cleanseMethod(cnode.name, mnode) // remove unreachable code
        }
      }
    }

  } // end of class EssentialCleanser

} // end of class BCodeOptIntra
