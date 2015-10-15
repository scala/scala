package scala.tools.nsc
package backend.jvm
package analysis

import scala.annotation.switch
import scala.tools.asm.{Handle, Type, Label}
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis.{Frame, BasicInterpreter, Analyzer, Value}
import scala.tools.nsc.backend.jvm.BTypes._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
import java.lang.invoke.LambdaMetafactory
import scala.collection.mutable
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
    import btypes.coreBTypes._

    // Make sure to reference the ClassBTypes of all types that are used in the code generated
    // here (e.g. java/util/Map) are initialized. Initializing a ClassBType adds it to the
    // `classBTypeFromInternalName` map. When writing the classfile, the asm ClassWriter computes
    // stack map frames and invokes the `getCommonSuperClass` method. This method expects all
    // ClassBTypes mentioned in the source code to exist in the map.

    val mapDesc = javaUtilMapReference.descriptor
    val nilLookupDesc = MethodBType(Nil, jliMethodHandlesLookup).descriptor
    val serlamObjDesc = MethodBType(jliSerializedLambda :: Nil, ObjectReference).descriptor
    val lookupMapSerlamObjDesc = MethodBType(jliMethodHandlesLookup :: javaUtilMapReference :: jliSerializedLambda :: Nil, ObjectReference).descriptor

    {
      val fv = cw.visitField(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambdaCache$", mapDesc, null, null)
      fv.visitEnd()
    }

    {
      val mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambda$", serlamObjDesc, null, null)
      mv.visitCode()
      // javaBinaryName returns the internal name of a class. Also used in BTypesFromsymbols.classBTypeFromSymbol.
      mv.visitFieldInsn(GETSTATIC, classNode.name, "$deserializeLambdaCache$", mapDesc)
      mv.visitVarInsn(ASTORE, 1)
      mv.visitVarInsn(ALOAD, 1)
      val l0 = new Label()
      mv.visitJumpInsn(IFNONNULL, l0)
      mv.visitTypeInsn(NEW, javaUtilHashMapReference.internalName)
      mv.visitInsn(DUP)
      mv.visitMethodInsn(INVOKESPECIAL, javaUtilHashMapReference.internalName, "<init>", "()V", false)
      mv.visitVarInsn(ASTORE, 1)
      mv.visitVarInsn(ALOAD, 1)
      mv.visitFieldInsn(PUTSTATIC, classNode.name, "$deserializeLambdaCache$", mapDesc)
      mv.visitLabel(l0)
      mv.visitFieldInsn(GETSTATIC, srLambdaDeserializer.internalName, "MODULE$", srLambdaDeserializer.descriptor)
      mv.visitMethodInsn(INVOKESTATIC, jliMethodHandles.internalName, "lookup", nilLookupDesc, false)
      mv.visitVarInsn(ALOAD, 1)
      mv.visitVarInsn(ALOAD, 0)
      mv.visitMethodInsn(INVOKEVIRTUAL, srLambdaDeserializer.internalName, "deserializeLambda", lookupMapSerlamObjDesc, false)
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

  /**
   * Visit the class node and collect all referenced nested classes.
   */
  def collectNestedClasses(classNode: ClassNode): List[ClassBType] = {
    val innerClasses = mutable.Set.empty[ClassBType]

    def visitInternalName(internalName: InternalName): Unit = if (internalName != null) {
      val t = classBTypeFromParsedClassfile(internalName)
      if (t.isNestedClass.get) innerClasses += t
    }

    // either an internal/Name or [[Linternal/Name; -- there are certain references in classfiles
    // that are either an internal name (without the surrounding `L;`) or an array descriptor
    // `[Linternal/Name;`.
    def visitInternalNameOrArrayReference(ref: String): Unit = if (ref != null) {
      val bracket = ref.lastIndexOf('[')
      if (bracket == -1) visitInternalName(ref)
      else if (ref.charAt(bracket + 1) == 'L') visitInternalName(ref.substring(bracket + 2, ref.length - 1))
    }

    // we are only interested in the class references in the descriptor, so we can skip over
    // primitves and the brackets of array descriptors
    def visitDescriptor(desc: String): Unit = (desc.charAt(0): @switch) match {
      case '(' =>
        val internalNames = mutable.ListBuffer.empty[String]
        var i = 1
        while (i < desc.length) {
          if (desc.charAt(i) == 'L') {
            val start = i + 1 // skip the L
            while (desc.charAt(i) != ';') i += 1
            internalNames append desc.substring(start, i)
          }
          // skips over '[', ')', primitives
          i += 1
        }
        internalNames foreach visitInternalName

      case 'L' =>
        visitInternalName(desc.substring(1, desc.length - 1))

      case '[' =>
        visitInternalNameOrArrayReference(desc)

      case _ => // skip over primitive types
    }

    def visitConstant(const: AnyRef): Unit = const match {
      case t: Type => visitDescriptor(t.getDescriptor)
      case _ =>
    }

    // in principle we could references to annotation types, as they only end up as strings in the
    // constant pool, not as class references. however, the java compiler still includes nested
    // annotation classes in the innerClass table, so we do the same. explained in detail in the
    // large comment in class BTypes.
    def visitAnnotation(annot: AnnotationNode): Unit = {
      visitDescriptor(annot.desc)
      if (annot.values != null) annot.values.asScala foreach visitConstant
    }

    def visitAnnotations(annots: java.util.List[_ <: AnnotationNode]) = if (annots != null) annots.asScala foreach visitAnnotation
    def visitAnnotationss(annotss: Array[java.util.List[AnnotationNode]]) = if (annotss != null) annotss foreach visitAnnotations

    def visitHandle(handle: Handle): Unit = {
      visitInternalNameOrArrayReference(handle.getOwner)
      visitDescriptor(handle.getDesc)
    }

    visitInternalName(classNode.name)
    innerClasses ++= classBTypeFromParsedClassfile(classNode.name).info.get.nestedClasses

    visitInternalName(classNode.superName)
    classNode.interfaces.asScala foreach visitInternalName
    visitInternalName(classNode.outerClass)

    visitAnnotations(classNode.visibleAnnotations)
    visitAnnotations(classNode.visibleTypeAnnotations)
    visitAnnotations(classNode.invisibleAnnotations)
    visitAnnotations(classNode.invisibleTypeAnnotations)

    for (f <- classNode.fields.asScala) {
      visitDescriptor(f.desc)
      visitAnnotations(f.visibleAnnotations)
      visitAnnotations(f.visibleTypeAnnotations)
      visitAnnotations(f.invisibleAnnotations)
      visitAnnotations(f.invisibleTypeAnnotations)
    }

    for (m <- classNode.methods.asScala) {
      visitDescriptor(m.desc)

      visitAnnotations(m.visibleAnnotations)
      visitAnnotations(m.visibleTypeAnnotations)
      visitAnnotations(m.invisibleAnnotations)
      visitAnnotations(m.invisibleTypeAnnotations)
      visitAnnotationss(m.visibleParameterAnnotations)
      visitAnnotationss(m.invisibleParameterAnnotations)
      visitAnnotations(m.visibleLocalVariableAnnotations)
      visitAnnotations(m.invisibleLocalVariableAnnotations)

      m.exceptions.asScala foreach visitInternalName
      for (tcb <- m.tryCatchBlocks.asScala) visitInternalName(tcb.`type`)

      val iter = m.instructions.iterator()
      while (iter.hasNext) iter.next() match {
        case ti: TypeInsnNode           => visitInternalNameOrArrayReference(ti.desc)
        case fi: FieldInsnNode          => visitInternalNameOrArrayReference(fi.owner); visitDescriptor(fi.desc)
        case mi: MethodInsnNode         => visitInternalNameOrArrayReference(mi.owner); visitDescriptor(mi.desc)
        case id: InvokeDynamicInsnNode  => visitDescriptor(id.desc); visitHandle(id.bsm); id.bsmArgs foreach visitConstant
        case ci: LdcInsnNode            => visitConstant(ci.cst)
        case ma: MultiANewArrayInsnNode => visitDescriptor(ma.desc)
        case _ =>
      }
    }
    innerClasses.toList
  }
}
