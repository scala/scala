package scala.tools.nsc
package backend.jvm
package analysis

import scala.tools.asm.Label
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis.{Frame, BasicInterpreter, Analyzer, Value}
import scala.tools.nsc.backend.jvm.BTypes._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
import java.lang.invoke.LambdaMetafactory
import scala.collection.convert.decorateAsJava._
import scala.collection.convert.decorateAsScala._

/**
 * This component hosts tools and utilities used in the backend that require access to a `BTypes`
 * instance.
 *
 * One example is the AsmAnalyzer class, which runs `computeMaxLocalsMaxStack` on the methodNode to
 * be analyzed. This method in turn lives inside the BTypes assembly because it queries the per-run
 * cache `maxLocalsMaxStackComputed` defined in there.
 */
class BackendUtils[BT <: BTypes](val btypes: BT) {
  import btypes._

  /**
   * A wrapper to make ASM's Analyzer a bit easier to use.
   */
  class AsmAnalyzer[V <: Value](methodNode: MethodNode, classInternalName: InternalName, val analyzer: Analyzer[V] = new Analyzer(new BasicInterpreter)) {
    localOpt.computeMaxLocalsMaxStack(methodNode)
    analyzer.analyze(classInternalName, methodNode)
    def frameAt(instruction: AbstractInsnNode): Frame[V] = analyzer.frameAt(instruction, methodNode)
  }

  /**
   * See the doc comment on package object `analysis` for a discussion on performance.
   */
  object AsmAnalyzer {
    // jvm limit is 65535 for both number of instructions and number of locals

    private def size(method: MethodNode) = method.instructions.size.toLong * method.maxLocals * method.maxLocals

    // with the limits below, analysis should not take more than one second

    private val nullnessSizeLimit    = 5000l * 600l  * 600l    // 5000 insns, 600 locals
    private val basicValueSizeLimit  = 9000l * 1000l * 1000l
    private val sourceValueSizeLimit = 8000l * 950l  * 950l

    def sizeOKForNullness(method: MethodNode): Boolean = size(method) < nullnessSizeLimit
    def sizeOKForBasicValue(method: MethodNode): Boolean = size(method) < basicValueSizeLimit
    def sizeOKForSourceValue(method: MethodNode): Boolean = size(method) < sourceValueSizeLimit
  }

  class ProdConsAnalyzer(val methodNode: MethodNode, classInternalName: InternalName) extends AsmAnalyzer(methodNode, classInternalName, new Analyzer(new InitialProducerSourceInterpreter)) with ProdConsAnalyzerImpl

  /**
   * Add:
   * private static java.util.Map $deserializeLambdaCache$ = null
   * private static Object $deserializeLambda$(SerializedLambda l) {
   *   var cache = $deserializeLambdaCache$
   *   if (cache eq null) {
   *     cache = new java.util.HashMap()
   *     $deserializeLambdaCache$ = cache
   *   }
   *   return scala.runtime.LambdaDeserializer.deserializeLambda(MethodHandles.lookup(), cache, l);
   * }
   */
  def addLambdaDeserialize(classNode: ClassNode): Unit = {
    val cw = classNode
    import scala.tools.asm.Opcodes._

    // Need to force creation of BTypes for these as `getCommonSuperClass` is called on
    // automatically computing the max stack size (`visitMaxs`) during method writing.
    btypes.coreBTypes.javaUtilHashMapReference
    btypes.coreBTypes.javaUtilMapReference

    // This is fine, even if `visitInnerClass` was called before for MethodHandles.Lookup: duplicates are not emitted.
    cw.visitInnerClass("java/lang/invoke/MethodHandles$Lookup", "java/lang/invoke/MethodHandles", "Lookup", ACC_PUBLIC + ACC_FINAL + ACC_STATIC)

    {
      val fv = cw.visitField(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambdaCache$", "Ljava/util/Map;", null, null)
      fv.visitEnd()
    }

    {
      val mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambda$", "(Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;", null, null)
      mv.visitCode()
      // javaBinaryName returns the internal name of a class. Also used in BTypesFromsymbols.classBTypeFromSymbol.
      mv.visitFieldInsn(GETSTATIC, classNode.name, "$deserializeLambdaCache$", "Ljava/util/Map;")
      mv.visitVarInsn(ASTORE, 1)
      mv.visitVarInsn(ALOAD, 1)
      val l0 = new Label()
      mv.visitJumpInsn(IFNONNULL, l0)
      mv.visitTypeInsn(NEW, "java/util/HashMap")
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, "java/util/HashMap", "<init>", "()V", false)
      mv.visitVarInsn(ASTORE, 1)
      mv.visitVarInsn(ALOAD, 1)
      mv.visitFieldInsn(PUTSTATIC, classNode.name, "$deserializeLambdaCache$", "Ljava/util/Map;")
      mv.visitLabel(l0)
      mv.visitFieldInsn(GETSTATIC, "scala/runtime/LambdaDeserializer$", "MODULE$", "Lscala/runtime/LambdaDeserializer$;")
      mv.visitMethodInsn(INVOKESTATIC, "java/lang/invoke/MethodHandles", "lookup", "()Ljava/lang/invoke/MethodHandles$Lookup;", false)
      mv.visitVarInsn(ALOAD, 1)
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKEVIRTUAL, "scala/runtime/LambdaDeserializer$", "deserializeLambda", "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/util/Map;Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;", false)
      mv.visitInsn(ARETURN)
      mv.visitEnd()
    }
  }

  /**
   * Clone the instructions in `methodNode` into a new [[InsnList]], mapping labels according to
   * the `labelMap`. Returns the new instruction list and a map from old to new instructions, and
   * a boolean indicating if the instruction list contains an instantiation of a serializable SAM
   * type.
   */
  def cloneInstructions(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode]): (InsnList, Map[AbstractInsnNode, AbstractInsnNode], Boolean) = {
    val javaLabelMap = labelMap.asJava
    val result = new InsnList
    var map = Map.empty[AbstractInsnNode, AbstractInsnNode]
    var hasSerializableClosureInstantiation = false
    for (ins <- methodNode.instructions.iterator.asScala) {
      if (!hasSerializableClosureInstantiation) ins match {
        case callGraph.LambdaMetaFactoryCall(indy, _, _, _) => indy.bsmArgs match {
          case Array(_, _, _, flags: Integer, xs@_*) if (flags.intValue & LambdaMetafactory.FLAG_SERIALIZABLE) != 0 =>
            hasSerializableClosureInstantiation = true
          case _ =>
        }
        case _ =>
      }
      val cloned = ins.clone(javaLabelMap)
      result add cloned
      map += ((ins, cloned))
    }
    (result, map, hasSerializableClosureInstantiation)
  }
}
