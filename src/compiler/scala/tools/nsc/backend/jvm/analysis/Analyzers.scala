package scala.tools.nsc
package backend.jvm
package analysis

import scala.tools.asm.tree.{AbstractInsnNode, MethodNode}
import scala.tools.asm.tree.analysis.{Frame, BasicInterpreter, Analyzer, Value}
import scala.tools.nsc.backend.jvm.BTypes._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._

/**
 * This component hosts tools for running ASM analyzers that require access to a `BTypes` instance.
 * In particular, the AsmAnalyzer class runs `computeMaxLocalsMaxStack` on the methodNode to be
 * analyzed. This method in turn lives inside the BTypes assembly because it queries the per-run
 * cache `maxLocalsMaxStackComputed` defined in there.
 */
class Analyzers[BT <: BTypes](val btypes: BT) {
  import btypes._

  /**
   * A wrapper to make ASM's Analyzer a bit easier to use.
   */
  class AsmAnalyzer[V <: Value](methodNode: MethodNode, classInternalName: InternalName, val analyzer: Analyzer[V] = new Analyzer(new BasicInterpreter)) {
    localOpt.computeMaxLocalsMaxStack(methodNode)
    analyzer.analyze(classInternalName, methodNode)
    def frameAt(instruction: AbstractInsnNode): Frame[V] = analyzer.frameAt(instruction, methodNode)
  }

  class ProdConsAnalyzer(val methodNode: MethodNode, classInternalName: InternalName) extends AsmAnalyzer(methodNode, classInternalName, new Analyzer(new InitialProducerSourceInterpreter)) with ProdConsAnalyzerImpl
}
