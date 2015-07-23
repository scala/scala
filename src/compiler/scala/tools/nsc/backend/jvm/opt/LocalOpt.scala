/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.switch
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.analysis.{Analyzer, BasicInterpreter}
import scala.tools.asm.tree._
import scala.collection.convert.decorateAsScala._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

/**
 * Optimizations within a single method.
 *
 * unreachable code
 *   - removes instructions of basic blocks to which no branch instruction points
 *   + enables eliminating some exception handlers and local variable descriptors
 *     > eliminating them is required for correctness, as explained in `removeUnreachableCode`
 *
 * empty exception handlers
 *   - removes exception handlers whose try block is empty
 *   + eliminating a handler where the try block is empty and reachable will turn the catch block
 *     unreachable. in this case "unreachable code" is invoked recursively until reaching a fixpoint.
 *     > for try blocks that are unreachable, "unreachable code" removes also the instructions of the
 *       catch block, and the recursive invocation is not necessary.
 *
 * simplify jumps
 *   - various simplifications, see doc comments of individual optimizations
 *   + changing or eliminating jumps may render some code unreachable, therefore "simplify jumps" is
 *     executed in a loop with "unreachable code"
 *
 * empty local variable descriptors
 *   - removes entries from the local variable table where the variable is not actually used
 *   + enables eliminating labels that the entry points to (if they are not otherwise referenced)
 *
 * empty line numbers
 *   - eliminates line number nodes that describe no executable instructions
 *   + enables eliminating the label of the line number node (if it's not otherwise referenced)
 *
 * stale labels
 *   - eliminate labels that are not referenced, merge sequences of label definitions.
 */
class LocalOpt[BT <: BTypes](val btypes: BT) {
  import LocalOptImpls._
  import btypes._

  /**
   * Remove unreachable code from a method.
   *
   * This implementation only removes instructions that are unreachable for an ASM analyzer /
   * interpreter. This ensures that future analyses will not produce `null` frames. The inliner
   * and call graph builder depend on this property.
   *
   * @return A set containing the eliminated instructions
   */
  def minimalRemoveUnreachableCode(method: MethodNode, ownerClassName: InternalName): Set[AbstractInsnNode] = {
    if (method.instructions.size == 0) return Set.empty     // fast path for abstract methods
    if (unreachableCodeEliminated(method)) return Set.empty // we know there is no unreachable code

    // For correctness, after removing unreachable code, we have to eliminate empty exception
    // handlers, see scaladoc of def methodOptimizations. Removing an live handler may render more
    // code unreachable and therefore requires running another round.
    def removalRound(): Set[AbstractInsnNode] = {
      val (removedInstructions, liveLabels) = removeUnreachableCodeImpl(method, ownerClassName)
      val removedRecursively = if (removedInstructions.nonEmpty) {
        val liveHandlerRemoved = removeEmptyExceptionHandlers(method).exists(h => liveLabels(h.start))
        if (liveHandlerRemoved) removalRound()
        else Set.empty
      } else Set.empty
      removedInstructions ++ removedRecursively
    }

    val removedInstructions = removalRound()
    if (removedInstructions.nonEmpty) removeUnusedLocalVariableNodes(method)()
    unreachableCodeEliminated += method
    removedInstructions
  }

  /**
   * Remove unreachable instructions from all (non-abstract) methods and apply various other
   * cleanups to the bytecode.
   *
   * @param clazz The class whose methods are optimized
   * @return      `true` if unreachable code was eliminated in some method, `false` otherwise.
   */
  def methodOptimizations(clazz: ClassNode): Boolean = {
    !compilerSettings.YoptNone && clazz.methods.asScala.foldLeft(false) {
      case (changed, method) => methodOptimizations(method, clazz.name) || changed
    }
  }

  /**
   * Remove unreachable code from a method.
   *
   * We rely on dead code elimination provided by the ASM framework, as described in the ASM User
   * Guide (http://asm.ow2.org/index.html), Section 8.2.1. It runs a data flow analysis, which only
   * computes Frame information for reachable instructions. Instructions for which no Frame data is
   * available after the analysis are unreachable.
   *
   * Also simplifies branching instructions, removes unused local variable descriptors, empty
   * exception handlers, unnecessary label declarations and empty line number nodes.
   *
   * Returns `true` if the bytecode of `method` was changed.
   */
  def methodOptimizations(method: MethodNode, ownerClassName: InternalName): Boolean = {
    if (method.instructions.size == 0) return false // fast path for abstract methods

    // unreachable-code also removes unused local variable nodes and empty exception handlers.
    // This is required for correctness, for example:
    //
    //   def f = { return 0; try { 1 } catch { case _ => 2 } }
    //
    // The result after removeUnreachableCodeImpl:
    //
    //   TRYCATCHBLOCK L0 L1 L2 java/lang/Exception
    //   L4
    //     ICONST_0
    //     IRETURN
    //   L0
    //   L1
    //   L2
    //
    // If we don't eliminate the handler, the ClassWriter emits:
    //
    //   TRYCATCHBLOCK L0 L0 L0 java/lang/Exception
    //   L1
    //     ICONST_0
    //     IRETURN
    //   L0
    //
    // This triggers "ClassFormatError: Illegal exception table range in class file C". Similar
    // for local variables in dead blocks. Maybe that's a bug in the ASM framework.

    def removalRound(): Boolean = {
      // unreachable-code, empty-handlers and simplify-jumps run until reaching a fixpoint (see doc on class LocalOpt)
      val (codeRemoved, handlersRemoved, liveHandlerRemoved) = if (compilerSettings.YoptUnreachableCode) {
        val (removedInstructions, liveLabels) = removeUnreachableCodeImpl(method, ownerClassName)
        val removedHandlers = removeEmptyExceptionHandlers(method)
        (removedInstructions.nonEmpty, removedHandlers.nonEmpty, removedHandlers.exists(h => liveLabels(h.start)))
      } else {
        (false, false, false)
      }

      val jumpsChanged = if (compilerSettings.YoptSimplifyJumps) simplifyJumps(method) else false

      // Eliminating live handlers and simplifying jump instructions may render more code
      // unreachable, so we need to run another round.
      if (liveHandlerRemoved || jumpsChanged) removalRound()

      codeRemoved || handlersRemoved || jumpsChanged
    }

    val codeHandlersOrJumpsChanged = removalRound()

    // (*) Removing stale local variable descriptors is required for correctness of unreachable-code
    val localsRemoved =
      if (compilerSettings.YoptCompactLocals) compactLocalVariables(method) // also removes unused
      else if (compilerSettings.YoptUnreachableCode) removeUnusedLocalVariableNodes(method)() // (*)
      else false

    val lineNumbersRemoved = if (compilerSettings.YoptEmptyLineNumbers) removeEmptyLineNumbers(method) else false

    val labelsRemoved = if (compilerSettings.YoptEmptyLabels) removeEmptyLabelNodes(method) else false

    // assert that local variable annotations are empty (we don't emit them) - otherwise we'd have
    // to eliminate those covering an empty range, similar to removeUnusedLocalVariableNodes.
    def nullOrEmpty[T](l: java.util.List[T]) = l == null || l.isEmpty
    assert(nullOrEmpty(method.visibleLocalVariableAnnotations), method.visibleLocalVariableAnnotations)
    assert(nullOrEmpty(method.invisibleLocalVariableAnnotations), method.invisibleLocalVariableAnnotations)

    unreachableCodeEliminated += method

    codeHandlersOrJumpsChanged || localsRemoved || lineNumbersRemoved || labelsRemoved
  }

}

object LocalOptImpls {
  /**
   * Removes unreachable basic blocks.
   *
   * TODO: rewrite, don't use computeMaxLocalsMaxStack (runs a ClassWriter) / Analyzer. Too slow.
   *
   * @return A set containing eliminated instructions, and a set containing all live label nodes.
   */
  def removeUnreachableCodeImpl(method: MethodNode, ownerClassName: InternalName): (Set[AbstractInsnNode], Set[LabelNode]) = {
    // The data flow analysis requires the maxLocals / maxStack fields of the method to be computed.
    computeMaxLocalsMaxStack(method)
    val a = new Analyzer(new BasicInterpreter)
    a.analyze(ownerClassName, method)
    val frames = a.getFrames

    val initialSize = method.instructions.size
    var i = 0
    var liveLabels = Set.empty[LabelNode]
    var removedInstructions = Set.empty[AbstractInsnNode]
    val itr = method.instructions.iterator()
    while (itr.hasNext) {
      itr.next() match {
        case l: LabelNode =>
          if (frames(i) != null) liveLabels += l

        case ins =>
          // label nodes are not removed: they might be referenced for example in a LocalVariableNode
          if (frames(i) == null || ins.getOpcode == Opcodes.NOP) {
            // Instruction iterators allow removing during iteration.
            // Removing is O(1): instructions are doubly linked list elements.
            itr.remove()
            removedInstructions += ins
          }
      }
      i += 1
    }
    (removedInstructions, liveLabels)
  }

  /**
   * Remove exception handlers that cover empty code blocks. A block is considered empty if it
   * consist only of labels, frames, line numbers, nops and gotos.
   *
   * There are no executable instructions that we can assume don't throw (eg ILOAD). The JVM spec
   * basically says that a VirtualMachineError may be thrown at any time:
   *   http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.3
   *
   * Note that no instructions are eliminated.
   *
   * @return the set of removed handlers
   */
  def removeEmptyExceptionHandlers(method: MethodNode): Set[TryCatchBlockNode] = {
    /** True if there exists code between start and end. */
    def containsExecutableCode(start: AbstractInsnNode, end: LabelNode): Boolean = {
      start != end && ((start.getOpcode : @switch) match {
        // FrameNode, LabelNode and LineNumberNode have opcode == -1.
        case -1 | Opcodes.GOTO => containsExecutableCode(start.getNext, end)
        case _ => true
      })
    }

    var removedHandlers = Set.empty[TryCatchBlockNode]
    val handlersIter = method.tryCatchBlocks.iterator()
    while(handlersIter.hasNext) {
      val handler = handlersIter.next()
      if (!containsExecutableCode(handler.start, handler.end)) {
        removedHandlers += handler
        handlersIter.remove()
      }
    }
    removedHandlers
  }

  /**
   * Remove all non-parameter entries from the local variable table which denote variables that are
   * not actually read or written.
   *
   * Note that each entry in the local variable table has a start, end and index. Two entries with
   * the same index, but distinct start / end ranges are different variables, they may have not the
   * same type or name.
   */
  def removeUnusedLocalVariableNodes(method: MethodNode)(firstLocalIndex: Int = parametersSize(method), renumber: Int => Int = identity): Boolean = {
    def variableIsUsed(start: AbstractInsnNode, end: LabelNode, varIndex: Int): Boolean = {
      start != end && (start match {
        case v: VarInsnNode if v.`var` == varIndex => true
        case _ => variableIsUsed(start.getNext, end, varIndex)
      })
    }

    val initialNumVars = method.localVariables.size
    val localsIter = method.localVariables.iterator()
    while (localsIter.hasNext) {
      val local = localsIter.next()
      val index = local.index
      // parameters and `this` (the lowest indices, starting at 0) are never removed or renumbered
      if (index >= firstLocalIndex) {
        if (!variableIsUsed(local.start, local.end, index)) localsIter.remove()
        else if (renumber(index) != index) local.index = renumber(index)
      }
    }
    method.localVariables.size != initialNumVars
  }

  /**
   * The number of local variable slots used for parameters and for the `this` reference.
   */
  private def parametersSize(method: MethodNode): Int = {
    // Double / long fields occupy two slots, so we sum up the sizes. Since getSize returns 0 for
    // void, we have to add `max 1`.
    val paramsSize = scala.tools.asm.Type.getArgumentTypes(method.desc).iterator.map(_.getSize max 1).sum
    val thisSize   = if ((method.access & Opcodes.ACC_STATIC) == 0) 1 else 0
    paramsSize + thisSize
  }

  /**
   * Compact the local variable slots used in the method's implementation. This prevents having
   * unused slots for example after eliminating unreachable code.
   *
   * This transformation reduces the size of the frame for invoking the method. For example, if the
   * method has an ISTORE instruction to the local variable 3, the maxLocals of the method is at
   * least 4, even if some local variable slots below 3 are not used by any instruction.
   *
   * This could be improved by doing proper register allocation.
   */
  def compactLocalVariables(method: MethodNode): Boolean = {
    // This array is built up to map local variable indices from old to new.
    val renumber = collection.mutable.ArrayBuffer.empty[Int]

    // Add the index of the local variable used by `varIns` to the `renumber` array.
    def addVar(varIns: VarInsnNode): Unit = {
      val index = varIns.`var`
      val isWide = (varIns.getOpcode: @switch) match {
        case Opcodes.LLOAD | Opcodes.DLOAD | Opcodes.LSTORE | Opcodes.DSTORE => true
        case _ => false
      }

      // Ensure the length of `renumber`. Unused variable indices are mapped to -1.
      val minLength = if (isWide) index + 2 else index + 1
      for (i <- renumber.length until minLength) renumber += -1

      renumber(index) = index
      if (isWide) renumber(index + 1) = index
    }

    // first phase: collect all used local variables. if the variable at index x is used, set
    // renumber(x) = x, otherwise renumber(x) = -1. if the variable is wide (long or double), set
    // renumber(x+1) = x.

    val firstLocalIndex = parametersSize(method)
    for (i <- 0 until firstLocalIndex) renumber += i // parameters and `this` are always used.
    method.instructions.iterator().asScala foreach {
      case VarInstruction(varIns) => addVar(varIns)
      case _ =>
    }

    // assign the next free slot to each used local variable.
    // for example, rewrite (0, 1, -1, 3, -1, 5) to (0, 1, -1, 2, -1, 3).

    var nextIndex = firstLocalIndex
    for (i <- firstLocalIndex until renumber.length if renumber(i) != -1) {
      renumber(i) = nextIndex
      nextIndex += 1
    }

    // Update the local variable descriptors according to the renumber table, and eliminate stale entries
    val removedLocalVariableDescriptors = removeUnusedLocalVariableNodes(method)(firstLocalIndex, renumber)

    if (nextIndex == renumber.length) removedLocalVariableDescriptors
    else {
      // update variable instructions according to the renumber table
      method.maxLocals = nextIndex
      method.instructions.iterator().asScala.foreach {
        case VarInstruction(varIns) =>
          val oldIndex = varIns.`var`
          if (oldIndex >= firstLocalIndex && renumber(oldIndex) != oldIndex)
            varIns.`var` = renumber(varIns.`var`)
        case _ =>
      }
      true
    }
  }

  /**
   * Removes LineNumberNodes that don't describe any executable instructions.
   *
   * This method expects (and asserts) that the `start` label of each LineNumberNode is the
   * lexically preceding label declaration.
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

    val allHandlers = method.tryCatchBlocks.asScala.toSet

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
          activeHandlers ++= allHandlers.filter(_.start == l)
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
   *   => Jump n;  [rest unchanged]
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
   * Note that no label definitions are allowed in the first [nops] section. Otherwise, there could
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
   * rewrite might change the behavior. For xRETURN, the reason is that return instructions may throw
   * an IllegalMonitorStateException, as described here:
   *   http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html#jvms-6.5.return
   */
  private def simplifyGotoReturn(method: MethodNode, instruction: AbstractInsnNode, inTryBlock: Boolean): Boolean = !inTryBlock && (instruction match {
    case Goto(jump) =>
      nextExecutableInstruction(jump.label) match {
        case Some(target) =>
          if (isReturn(target) || target.getOpcode == Opcodes.ATHROW) {
            method.instructions.set(jump, target.clone(null))
            true
          } else false

        case _ => false
      }
    case _ => false
  })
}
