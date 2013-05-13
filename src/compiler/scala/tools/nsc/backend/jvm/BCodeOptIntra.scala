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
abstract class BCodeOptIntra extends BCodeSyncAndTry {

  import global._

  /*
   *  SI-6720: Avoid java.lang.VerifyError: Uninitialized object exists on backward branch.
   *
   *  Quoting from the JVM Spec, 4.9.2 Structural Constraints , http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html
   *
   *     There must never be an uninitialized class instance on the operand stack or in a local variable
   *     at the target of a backwards branch unless the special type of the uninitialized class instance
   *     at the branch instruction is merged with itself at the target of the branch (Sec. 4.10.2.4).
   *
   *  The Oracle JVM as of JDK 7 has started rejecting bytecode of the form:
   *
   *      NEW x
   *      DUP
   *      ... instructions loading ctor-args, involving a backedge
   *      INVOKESPECIAL <init>
   *
   *  `rephraseBackedgesInConstructorArgs()` overcomes the above by reformulating into:
   *
   *      ... instructions loading ctor-arg N
   *      STORE nth-arg
   *      ... instructions loading ctor-arg (N-1)
   *      STORE (n-1)th-arg
   *      ... and so on
   *      STORE 1st-arg
   *      NEW x
   *      DUP
   *      LOAD 1st-arg
   *      ...
   *      LOAD nth-arg
   *      INVOKESPECIAL <init>
   *
   *  A warning informs that, in the rewritten version, `NEW x` comes after the code to compute arguments.
   *  It's either that (potential) behavioral change or VerifyError.
   *  "Behavioral change" that is, in case the class being instantiated has a side-effecting static initializer.
   *
   *  @param nxtIdx0  next available index for local variable
   *  @param bes      backedges in ctor-arg section
   *  @param newInsn  left  bracket of the section where backedges were found
   *  @param initInsn right bracket of the section where backedges were found
   *
   */
  def rephraseBackedgesInCtorArg(nxtIdx0:  Int,
                                 bes:      _root_.java.util.Map[asm.tree.JumpInsnNode, asm.tree.LabelNode],
                                 cnode:    asm.tree.ClassNode,
                                 mnode:    asm.tree.MethodNode,
                                 newInsn:  asm.tree.TypeInsnNode,
                                 initInsn: MethodInsnNode): Int = {

    var nxtIdx = nxtIdx0

        def methodSignature(cnode: ClassNode, mnode: MethodNode): String = {
          cnode.name + "::" + mnode.name + mnode.desc
        }

        def insnPos(insn: AbstractInsnNode, mnode: MethodNode): String = {
          s"${Util.textify(insn)} at index ${mnode.instructions.indexOf(insn)}"
        }

    import collection.convert.Wrappers.JSetWrapper
    for(
      entry <- JSetWrapper(bes.entrySet());
      jump  = entry.getKey;
      label = entry.getValue
    ) {
      warning(
        s"Backedge found in contructor-args section, in method ${methodSignature(cnode, mnode)} " +
        s"(jump ${insnPos(jump, mnode)} , target ${insnPos(label, mnode)} ). " +
        s"In order to avoid SI-6720, adding LOADs and STOREs for arguments. "  +
        s"As a result, ${newInsn.desc} is now instantiated after evaluating all ctor-arguments, " +
        s"on the assumption such class has not static-ctor performing visible side-effects that could make a difference."
      )
    }

    assert(newInsn.getOpcode == asm.Opcodes.NEW)
    val dupInsn = newInsn.getNext
    val paramTypes = BType.getMethodType(initInsn.desc).getArgumentTypes

    val stream = mnode.instructions
    stream.remove(newInsn)
    stream.remove(dupInsn)
    stream.insertBefore(initInsn, newInsn)
    stream.insertBefore(initInsn, dupInsn)

    for(i <- (paramTypes.length - 1) to 0 by -1) {
      val pt = paramTypes(i)
      val idxVar = nxtIdx
      nxtIdx += pt.getSize
      val load  = new asm.tree.VarInsnNode(pt.getOpcode(asm.Opcodes.ILOAD),  idxVar)
      val store = new asm.tree.VarInsnNode(pt.getOpcode(asm.Opcodes.ISTORE), idxVar)
      stream.insertBefore(newInsn, store)
      stream.insert(dupInsn,load)
    }

    nxtIdx
  } // end of method rephraseBackedgesInCtorArg()

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
