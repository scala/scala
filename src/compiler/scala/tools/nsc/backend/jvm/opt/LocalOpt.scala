/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.tools.asm.{Opcodes, MethodWriter, ClassWriter}
import scala.tools.asm.tree.analysis.{Analyzer, BasicValue, BasicInterpreter}
import scala.tools.asm.tree._
import scala.collection.convert.decorateAsScala._
import scala.collection.{ mutable => m }
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

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
      case (changed, method) => removeUnreachableCode(method, clazz.name) || changed
    }
  }

  /**
   * Remove unreachable code from a method.
   * We rely on dead code elimination provided by the ASM framework, as described in the ASM User
   * Guide (http://asm.ow2.org/index.html), Section 8.2.1. It runs a data flow analysis, which only
   * computes Frame information for reachable instructions. Instructions for which no Frame data is
   * available after the analyis are unreachable.
   *
   * TODO doc: it also removes empty handlers, unused local vars
   *
   * Returns `true` if dead code in `method` has been eliminated.
   */
  private def removeUnreachableCode(method: MethodNode, ownerClassName: String): Boolean = {
    if (method.instructions.size == 0) return false // fast path for abstract methods

    val codeRemoved = removeUnreachableCodeImpl(method, ownerClassName)

    // unreachable-code also removes unused local variable nodes and empty exception handlers.
    // This is required for correctness: such nodes are not allowed to refer to instruction offsets
    // that don't exist (because they have been eliminated).
    val localsRemoved = removeUnusedLocalVariableNodes(method)
    val handlersRemoved = removeEmptyExceptionHandlers(method)

    // When eliminating a handler, the catch block becomes unreachable. The recursive invocation
    // removes these blocks.
    // Note that invoking removeUnreachableCode*Impl* a second time is not enough: removing the dead
    // catch block can render other handlers empty, which also have to be removed in turn.
    if (handlersRemoved) removeUnreachableCode(method, ownerClassName)

    // assert that we can leave local variable annotations as-is
    def nullOrEmpty[T](l: java.util.List[T]) = l == null || l.isEmpty
    assert(nullOrEmpty(method.visibleLocalVariableAnnotations), method.visibleLocalVariableAnnotations)
    assert(nullOrEmpty(method.invisibleLocalVariableAnnotations), method.invisibleLocalVariableAnnotations)

    codeRemoved || localsRemoved || handlersRemoved
  }

  private def removeUnreachableCodeImpl(method: MethodNode, ownerClassName: String): Boolean = {
    val initialSize = method.instructions.size
    if (initialSize == 0) return false

    // The data flow analysis requires the maxLocals / maxStack fields of the method to be computed.
    computeMaxLocalsMaxStack(method)
    val a = new Analyzer[BasicValue](new BasicInterpreter)
    a.analyze(ownerClassName, method)
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

    method.instructions.size != initialSize
  }

  /**
   * Remove exception handlers that cover empty code blocks from all methods of `clazz`.
   * Returns `true` if any exception handler was eliminated.
   */
  def removeEmptyExceptionHandlers(clazz: ClassNode): Boolean = {
    clazz.methods.asScala.foldLeft(false) {
      case (changed, method) => removeEmptyExceptionHandlers(method) || changed
    }
  }

  /**
   * Remove exception handlers that cover empty code blocks. A block is considered empty if it
   * consist only of labels, frames, line numbers, nops and gotos.
   *
   * Note that no instructions are eliminated.
   *
   * @return `true` if some exception handler was eliminated.
   */
  def removeEmptyExceptionHandlers(method: MethodNode): Boolean = {
    /** True if there exists code between start and end. */
    def containsExecutableCode(start: AbstractInsnNode, end: LabelNode): Boolean = {
      start != end && (start.getOpcode match {
        // FrameNode, LabelNode and LineNumberNode have opcode == -1.
        case -1 | Opcodes.NOP | Opcodes.GOTO => containsExecutableCode(start.getNext, end)
        case _ => true
      })
    }

    val initialNumberHandlers = method.tryCatchBlocks.size
    val handlersIter = method.tryCatchBlocks.iterator()
    while(handlersIter.hasNext) {
      val handler = handlersIter.next()
      if (!containsExecutableCode(handler.start, handler.end)) handlersIter.remove()
    }
    method.tryCatchBlocks.size != initialNumberHandlers
  }

  /**
   * Remove all non-parameter entries from the local variable table which denote variables that are
   * not actually read or written.
   *
   * Note that each entry in the local variable table has a start, end and index. Two entries with
   * the same index, but distinct start / end ranges are different variables, they may have not the
   * same type or name.
   *
   * TODO: also re-allocate locals to occupy fewer slots after eliminating unused ones
   */
  def removeUnusedLocalVariableNodes(method: MethodNode): Boolean = {
    def variableIsUsed(start: AbstractInsnNode, end: LabelNode, varIndex: Int): Boolean = {
      start != end && (start match {
        case v: VarInsnNode =>  v.`var` == varIndex
        case _ => variableIsUsed(start.getNext, end, varIndex)
      })
    }

    val initialNumVars = method.localVariables.size
    val localsIter = method.localVariables.iterator()
    // The parameters and `this` (for instance methods) have the lowest indices in the local variables
    // table. Note that double / long fields occupy two slots, so we sum up the sizes. Since getSize
    // returns 0 for void, we have to add `max 1`.
    val paramsSize = scala.tools.asm.Type.getArgumentTypes(method.desc).map(_.getSize max 1).sum
    val thisSize = if ((method.access & Opcodes.ACC_STATIC) == 0) 1 else 0
    val endParamIndex = paramsSize + thisSize
    while (localsIter.hasNext) {
      val local = localsIter.next()
      // parameters and `this` have the lowest indices, starting at 0
      val used = local.index < endParamIndex || variableIsUsed(local.start, local.end, local.index)
      if (!used)
        localsIter.remove()
    }
    method.localVariables.size == initialNumVars
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

  /**
   * Removes LineNumberNodes that don't describe any executable instructions.
   *
   * This method expects (and asserts) that the `start` label of each LineNumberNode is the
   * lexically preceeding label declaration.
   */
  def removeEmptyLineNumbers(method: MethodNode): Boolean = {
    def isEmpty(node: AbstractInsnNode): Boolean = node.getNext match {
      case null => true
      case l: LineNumberNode => true
      case n if n.getOpcode >= 0 => false
      case n => isEmpty(n)
    }

    val initialSize = method.instructions.size
    val iterator = method.instructions.iterator()
    var previousLabel: LabelNode = null
    while (iterator.hasNext) {
      iterator.next match {
        case label: LabelNode => previousLabel = label
        case line: LineNumberNode if isEmpty(line) =>
          assert(line.start == previousLabel)
          iterator.remove()
        case _ =>
      }
    }
    method.instructions.size != initialSize
  }

  /**
   * Removes unreferenced label declarations, also squashes sequences of label definitions.
   *
   *      [ops];              Label(a); Label(b);  [ops];
   *   => subs([ops], b, a);  Label(a);            subs([ops], b, a);
   */
  def removeEmptyLabelNodes(method: MethodNode): Boolean = {
    val references = labelReferences(method)

    val initialSize = method.instructions.size
    val iterator = method.instructions.iterator()
    var prev: LabelNode = null
    while (iterator.hasNext) {
      iterator.next match {
        case label: LabelNode =>
          if (!references.contains(label)) iterator.remove()
          else if (prev != null) {
            references(label).foreach(substituteLabel(_, label, prev))
            iterator.remove()
          } else prev = label

        case instruction =>
          if (instruction.getOpcode >= 0) prev = null
      }
    }
    method.instructions.size != initialSize
  }

  /**
   * Apply various simplifications to branching instructions.
   */
  def simplifyJumps(method: MethodNode): Boolean = {
    var changed = false

    val allHanlders = method.tryCatchBlocks.asScala.toSet

    // A set of all exception handlers that guard the current instruction, required for simplifyGotoReturn
    var activeHandlers = Set.empty[TryCatchBlockNode]

    // Instructions that need to be removed. simplifyBranchOverGoto returns an instruction to be
    // removed. It cannot remove it itself because the instruction may be the successor of the current
    // instruction of the iterator, which is not supported in ASM.
    var instructionsToRemove = Set.empty[AbstractInsnNode]

    val iterator = method.instructions.iterator()
    while (iterator.hasNext) {
      val instruction = iterator.next()

      instruction match {
        case l: LabelNode =>
          activeHandlers ++= allHanlders.filter(_.start == l)
          activeHandlers = activeHandlers.filter(_.end != l)
        case _ =>
      }

      if (instructionsToRemove(instruction)) {
        iterator.remove()
        instructionsToRemove -= instruction
      } else if (isJumpNonJsr(instruction)) { // fast path - all of the below only treat jumps
        var jumpRemoved = simplifyThenElseSameTarget(method, instruction)

        if (!jumpRemoved) {
          changed = collapseJumpChains(instruction) || changed
          jumpRemoved = removeJumpToSuccessor(method, instruction)

          if (!jumpRemoved) {
            val staleGoto = simplifyBranchOverGoto(method, instruction)
            instructionsToRemove ++= staleGoto
            changed ||= staleGoto.nonEmpty
            changed = simplifyGotoReturn(method, instruction, inTryBlock = activeHandlers.nonEmpty) || changed
          }
        }
        changed ||= jumpRemoved
      }
    }
    assert(instructionsToRemove.isEmpty, "some optimization required removing a previously traversed instruction. add `instructionsToRemove.foreach(method.instructions.remove)`")
    changed
  }

  /**
   * Removes a conditional jump if it is followed by a GOTO to the same destination.
   *
   *      CondJump l;  [nops];  GOTO l;  [...]
   *      POP*;        [nops];  GOTO l;  [...]
   *
   * Introduces 1 or 2 POP instructions, depending on the number of values consumed by the CondJump.
   */
  private def simplifyThenElseSameTarget(method: MethodNode, instruction: AbstractInsnNode): Boolean = instruction match {
    case ConditionalJump(jump) =>
      nextExecutableInstruction(instruction) match {
        case Some(Goto(elseJump)) if sameTargetExecutableInstruction(jump, elseJump) =>
          removeJumpAndAdjustStack(method, jump)
          true

        case _ => false
      }
    case _ => false
  }

  /**
   * Replace jumps to a sequence of GOTO instructions by a jump to the final destination.
   *
   *      Jump l;  [any ops];  l: GOTO m;  [any ops];  m: GOTO n;  [any ops];   n: NotGOTO; [...]
   *   => Jump n;  [rest unchaned]
   *
   * If there's a loop of GOTOs, the initial jump is replaced by one of the labels in the loop.
   */
  private def collapseJumpChains(instruction: AbstractInsnNode): Boolean = instruction match {
    case JumpNonJsr(jump) =>
      val target = finalJumpTarget(jump)
      if (jump.label == target) false else {
        jump.label = target
        true
      }

    case _ => false
  }

  /**
   * Eliminates unnecessary jump instructions
   *
   *      Jump l;  [nops];  l: [...]
   *   => POP*;    [nops];  l: [...]
   *
   * Introduces 0, 1 or 2 POP instructions, depending on the number of values consumed by the Jump.
   */
  private def removeJumpToSuccessor(method: MethodNode, instruction: AbstractInsnNode) = instruction match {
    case JumpNonJsr(jump) if nextExecutableInstruction(jump, alsoKeep = Set(jump.label)) == Some(jump.label) =>
      removeJumpAndAdjustStack(method, jump)
      true
    case _ => false
  }

  /**
   * If the "else" part of a conditional branch is a simple GOTO, negates the conditional branch
   * and eliminates the GOTO.
   *
   *      CondJump l;         [nops, no labels];  GOTO m;  [nops];  l: [...]
   *   => NegatedCondJump m;  [nops, no labels];           [nops];  l: [...]
   *
   * Note that no label definitions are allowed in the first [nops] section. Otherwsie, there could
   * be some other jump to the GOTO, and eliminating it would change behavior.
   *
   * For technical reasons, we cannot remove the GOTO here (*).Instead this method returns an Option
   * containing the GOTO that needs to be eliminated.
   *
   * (*) The ASM instruction iterator (used in the caller [[simplifyJumps]]) has an undefined
   *     behavior if the successor of the current instruction is removed, which may be the case here
   */
  private def simplifyBranchOverGoto(method: MethodNode, instruction: AbstractInsnNode): Option[JumpInsnNode] = instruction match {
    case ConditionalJump(jump) =>
      // don't skip over labels, see doc comment
      nextExecutableInstruction(jump, alsoKeep = _.isInstanceOf[LabelNode]) match {
        case Some(Goto(goto)) =>
          if (nextExecutableInstruction(goto, alsoKeep = Set(jump.label)) == Some(jump.label)) {
            val newJump = new JumpInsnNode(negateJumpOpcode(jump.getOpcode), goto.label)
            method.instructions.set(jump, newJump)
            Some(goto)
          } else None

        case _ => None
      }
    case _ => None
  }

  /**
   * Inlines xRETURN and ATHROW
   *
   *      GOTO l;            [any ops];  l: xRETURN/ATHROW
   *   => xRETURN/ATHROW;    [any ops];  l: xRETURN/ATHROW
   *
   * inlining is only done if the GOTO instruction is not part of a try block, otherwise the
   * rewrite might change the behavior. For xRETURN, the reason is that return insructions may throw
   * an IllegalMonitorStateException, as described here:
   *   http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.return
   */
  private def simplifyGotoReturn(method: MethodNode, instruction: AbstractInsnNode, inTryBlock: Boolean): Boolean = !inTryBlock && (instruction match {
    case Goto(jump) =>
      nextExecutableInstruction(jump.label) match {
        case Some(target) =>
          val op = target.getOpcode
          if ((op >= Opcodes.IRETURN && op <= Opcodes.RETURN) || op == Opcodes.ATHROW) {
            method.instructions.set(jump, target.clone(null))
            true
          } else false

        case _ => false
      }
    case _ => false
  })
}
