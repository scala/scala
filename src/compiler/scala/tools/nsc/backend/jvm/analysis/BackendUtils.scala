/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend.jvm
package analysis

import java.lang.invoke.LambdaMetafactory

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import scala.collection.JavaConverters._
import java.util.concurrent.ConcurrentHashMap

import scala.tools.asm
import scala.tools.asm.Opcodes._
import scala.tools.asm.tree._
import scala.tools.asm.tree.analysis._
import scala.tools.asm.{Handle, Type}
import scala.tools.nsc.backend.jvm.BTypes._
import scala.tools.nsc.backend.jvm.GenBCode._
import scala.tools.nsc.backend.jvm.analysis.BackendUtils._
import scala.tools.nsc.backend.jvm.opt.BytecodeUtils._
import scala.util.control.{NoStackTrace, NonFatal}

/**
 * This component hosts tools and utilities used in the backend that require access to a `BTypes`
 * instance.
 *
 * One example is the AsmAnalyzer class, which runs `computeMaxLocalsMaxStack` on the methodNode to
 * be analyzed. This method in turn lives inside the BTypes assembly because it queries the per-run
 * cache `maxLocalsMaxStackComputed` defined in there.
 *
 * TODO: move out of `analysis` package?
 */
abstract class BackendUtils extends PerRunInit {
  val postProcessor: PostProcessor

  import postProcessor.{bTypes, bTypesFromClassfile, callGraph}
  import bTypes._
  import callGraph.ClosureInstantiation
  import coreBTypes._
  import frontendAccess.{compilerSettings, recordPerRunJavaMapCache}

  /**
   * Classes with indyLambda closure instantiations where the SAM type is serializable (e.g. Scala's
   * FunctionN) need a `$deserializeLambda$` method. This map contains classes for which such a
   * method has been generated. It is used during ordinary code generation, as well as during
   * inlining: when inlining an indyLambda instruction into a class, we need to make sure the class
   * has the method.
   */
  private val indyLambdaImplMethods: ConcurrentHashMap[InternalName, mutable.LinkedHashSet[asm.Handle]] = recordPerRunJavaMapCache{
    new ConcurrentHashMap[InternalName, mutable.LinkedHashSet[asm.Handle]]
  }

  // unused objects created by these constructors are eliminated by pushPop
  private[this] lazy val sideEffectFreeConstructors: LazyVar[Set[(String, String)]] = perRunLazy(this) {
    val ownerDesc = (p: (InternalName, MethodNameAndType)) => (p._1, p._2.methodType.descriptor)
    primitiveBoxConstructors.map(ownerDesc).toSet ++
      srRefConstructors.map(ownerDesc) ++
      tupleClassConstructors.map(ownerDesc) ++ Set(
      (ObjectRef.internalName, MethodBType(Nil, UNIT).descriptor),
      (StringRef.internalName, MethodBType(Nil, UNIT).descriptor),
      (StringRef.internalName, MethodBType(List(StringRef), UNIT).descriptor),
      (StringRef.internalName, MethodBType(List(ArrayBType(CHAR)), UNIT).descriptor))
  }

  private[this] lazy val classesOfSideEffectFreeConstructors: LazyVar[Set[String]] = perRunLazy(this)(sideEffectFreeConstructors.get.map(_._1))

  lazy val classfileVersion: LazyVar[Int] = perRunLazy(this)(compilerSettings.target match {
      case "8"  => asm.Opcodes.V1_8
      case "9"  => asm.Opcodes.V9
      case "10" => asm.Opcodes.V10
      case "11" => asm.Opcodes.V11
      case "12" => asm.Opcodes.V12
      case "13" => asm.Opcodes.V13
      case "14" => asm.Opcodes.V14
      case "15" => asm.Opcodes.V15
      case "16" => asm.Opcodes.V16
      case "17" => asm.Opcodes.V17
      case "18" => asm.Opcodes.V18
      case "19" => asm.Opcodes.V19
      case "20" => asm.Opcodes.V20
      case "21" => asm.Opcodes.V21
      case "22" => asm.Opcodes.V22
      case "23" => asm.Opcodes.V23
      // to be continued...
    })


  lazy val majorVersion: LazyVar[Int] = perRunLazy(this)(classfileVersion.get & 0xFF)

  lazy val emitStackMapFrame: LazyVar[Boolean] = perRunLazy(this)(majorVersion.get >= 50)

  lazy val extraProc: LazyVar[Int] = perRunLazy(this)(
    asm.ClassWriter.COMPUTE_MAXS |
      (if (emitStackMapFrame.get) asm.ClassWriter.COMPUTE_FRAMES else 0)
  )

  /**
   * A wrapper to make ASM's Analyzer a bit easier to use.
   */
  class AsmAnalyzer[V <: Value](methodNode: MethodNode, classInternalName: InternalName, val analyzer: Analyzer[V] = new Analyzer(new BasicInterpreter)) {
    computeMaxLocalsMaxStack(methodNode)
    try {
      analyzer.analyze(classInternalName, methodNode)
    } catch {
      case ae: AnalyzerException =>
        throw new AnalyzerException(null, "While processing " + classInternalName + "." + methodNode.name, ae)
    }
    def frameAt(instruction: AbstractInsnNode): Frame[V] = analyzer.frameAt(instruction, methodNode)
  }

  /**
   * See the doc comment on package object `analysis` for a discussion on performance.
   */
  object AsmAnalyzer {
    // jvm limit is 65535 for both number of instructions and number of locals

    private def size(method: MethodNode) = {
      val ml = maxLocals(method)
      method.instructions.size.toLong * ml * ml
    }

    // with the limits below, analysis should not take more than one second

    private val nullnessSizeLimit    = 5000l * 600l  * 600l    // 5000 insns, 600 locals
    private val basicValueSizeLimit  = 9000l * 1000l * 1000l
    private val sourceValueSizeLimit = 8000l * 950l  * 950l

    def sizeOKForAliasing(method: MethodNode): Boolean = size(method) < nullnessSizeLimit
    def sizeOKForNullness(method: MethodNode): Boolean = size(method) < nullnessSizeLimit
    def sizeOKForBasicValue(method: MethodNode): Boolean = size(method) < basicValueSizeLimit
    def sizeOKForSourceValue(method: MethodNode): Boolean = size(method) < sourceValueSizeLimit
  }

  class ProdConsAnalyzer(val methodNode: MethodNode, classInternalName: InternalName) extends AsmAnalyzer(methodNode, classInternalName, new Analyzer(new InitialProducerSourceInterpreter)) with ProdConsAnalyzerImpl

  class NonLubbingTypeFlowAnalyzer(val methodNode: MethodNode, classInternalName: InternalName) extends AsmAnalyzer(methodNode, classInternalName, new Analyzer(new NonLubbingTypeFlowInterpreter))

  /*
   * Add:
   *
   * private static Object $deserializeLambda$(SerializedLambda l) {
   *   try return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup$0](l)
   *   catch {
   *     case i: IllegalArgumentException =>
   *       try return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup$1](l)
   *       catch {
   *         case i: IllegalArgumentException =>
   *           ...
   *             return indy[scala.runtime.LambdaDeserialize.bootstrap, targetMethodGroup${NUM_GROUPS-1}](l)
   *       }
   *
   * We use invokedynamic here to enable caching within the deserializer without needing to
   * host a static field in the enclosing class. This allows us to add this method to interfaces
   * that define lambdas in default methods.
   *
   * SI-10232 we can't pass arbitrary number of method handles to the final varargs parameter of the bootstrap
   * method due to a limitation in the JVM. Instead, we emit a separate invokedynamic bytecode for each group of target
   * methods.
   */
  def addLambdaDeserialize(classNode: ClassNode, implMethods: Iterable[Handle]): Unit = {
    val cw = classNode

    // Make sure to reference the ClassBTypes of all types that are used in the code generated
    // here (e.g. java/util/Map) are initialized. Initializing a ClassBType adds it to the
    // `cachedClassBType` maps. When writing the classfile, the asm ClassWriter computes
    // stack map frames and invokes the `getCommonSuperClass` method. This method expects all
    // ClassBTypes mentioned in the source code to exist in the map.

    val serlamObjDesc = MethodBType(jliSerializedLambdaRef :: Nil, ObjectRef).descriptor
    val implMethodsArray = implMethods.toArray

    val mv = cw.visitMethod(ACC_PRIVATE + ACC_STATIC + ACC_SYNTHETIC, "$deserializeLambda$", serlamObjDesc, null, null)
    def emitLambdaDeserializeIndy(targetMethods: Seq[Handle]) {
      mv.visitVarInsn(ALOAD, 0)
      mv.visitInvokeDynamicInsn("lambdaDeserialize", serlamObjDesc, lambdaDeserializeBootstrapHandle, targetMethods: _*)
    }

    val targetMethodGroupLimit = 255 - 1 - 3 // JVM limit. See See MAX_MH_ARITY in CallSite.java
    val groups: Array[Array[Handle]] = implMethodsArray.grouped(targetMethodGroupLimit).toArray
    val numGroups = groups.length

    import scala.tools.asm.Label
    val initialLabels = Array.fill(numGroups - 1)(new Label())
    val terminalLabel = new Label
    def nextLabel(i: Int) = if (i == numGroups - 2) terminalLabel else initialLabels(i + 1)

    for ((label, i) <- initialLabels.iterator.zipWithIndex) {
      mv.visitTryCatchBlock(label, nextLabel(i), nextLabel(i), jlIllegalArgExceptionRef.internalName)
    }
    for ((label, i) <- initialLabels.iterator.zipWithIndex) {
      mv.visitLabel(label)
      emitLambdaDeserializeIndy(groups(i))
      mv.visitInsn(ARETURN)
    }
    mv.visitLabel(terminalLabel)
    emitLambdaDeserializeIndy(groups(numGroups - 1))
    mv.visitInsn(ARETURN)
  }

  /**
   * Clone the instructions in `methodNode` into a new [[InsnList]], mapping labels according to
   * the `labelMap`. Returns the new instruction list and a map from old to new instructions, and
   * a list of lambda implementation methods references by invokedynamic[LambdaMetafactory] for a
   * serializable SAM types.
   */
  def cloneInstructions(methodNode: MethodNode, labelMap: Map[LabelNode, LabelNode], keepLineNumbers: Boolean): (InsnList, Map[AbstractInsnNode, AbstractInsnNode], List[Handle]) = {
    val javaLabelMap = labelMap.asJava
    val result = new InsnList
    var map = Map.empty[AbstractInsnNode, AbstractInsnNode]
    val inlinedTargetHandles = mutable.ListBuffer[Handle]()
    for (ins <- methodNode.instructions.iterator.asScala) {
      ins match {
        case callGraph.LambdaMetaFactoryCall(indy, _, _, _) => indy.bsmArgs match {
          case Array(_, targetHandle: Handle, _, flags: Integer, xs@_*) if (flags.intValue & LambdaMetafactory.FLAG_SERIALIZABLE) != 0 =>
            inlinedTargetHandles += targetHandle
          case _ =>
        }
        case _ =>
      }
      if (keepLineNumbers || !ins.isInstanceOf[LineNumberNode]) {
        val cloned = ins.clone(javaLabelMap)
        result add cloned
        map += ((ins, cloned))
      }
    }
    (result, map, inlinedTargetHandles.toList)
  }

  def getBoxedUnit: FieldInsnNode = new FieldInsnNode(GETSTATIC, srBoxedUnitRef.internalName, "UNIT", srBoxedUnitRef.descriptor)

  private val anonfunAdaptedName = """.*\$anonfun\$.*\$\d+\$adapted""".r
  def hasAdaptedImplMethod(closureInit: ClosureInstantiation): Boolean = {
    anonfunAdaptedName.pattern.matcher(closureInit.lambdaMetaFactoryCall.implMethod.getName).matches
  }

  private def primitiveAsmTypeToBType(primitiveType: Type): PrimitiveBType = (primitiveType.getSort: @switch) match {
    case Type.BOOLEAN => BOOL
    case Type.BYTE    => BYTE
    case Type.CHAR    => CHAR
    case Type.SHORT   => SHORT
    case Type.INT     => INT
    case Type.LONG    => LONG
    case Type.FLOAT   => FLOAT
    case Type.DOUBLE  => DOUBLE
    case _            => null
  }

  def isScalaBox(insn: MethodInsnNode): Boolean = {
    insn.owner == srBoxesRunTimeRef.internalName && {
      val args = Type.getArgumentTypes(insn.desc)
      args.length == 1 && (srBoxesRuntimeBoxToMethods.get(primitiveAsmTypeToBType(args(0))) match {
        case Some(MethodNameAndType(name, tp)) => name == insn.name && tp.descriptor == insn.desc
        case _ => false
      })
    }
  }

  def getScalaBox(primitiveType: Type): MethodInsnNode = {
    val bType = primitiveAsmTypeToBType(primitiveType)
    val MethodNameAndType(name, methodBType) = srBoxesRuntimeBoxToMethods(bType)
    new MethodInsnNode(INVOKESTATIC, srBoxesRunTimeRef.internalName, name, methodBType.descriptor, /*itf =*/ false)
  }

  def isScalaUnbox(insn: MethodInsnNode): Boolean = {
    insn.owner == srBoxesRunTimeRef.internalName && (srBoxesRuntimeUnboxToMethods.get(primitiveAsmTypeToBType(Type.getReturnType(insn.desc))) match {
      case Some(MethodNameAndType(name, tp)) => name == insn.name && tp.descriptor == insn.desc
      case _ => false
    })
  }

  def getScalaUnbox(primitiveType: Type): MethodInsnNode = {
    val bType = primitiveAsmTypeToBType(primitiveType)
    val MethodNameAndType(name, methodBType) = srBoxesRuntimeUnboxToMethods(bType)
    new MethodInsnNode(INVOKESTATIC, srBoxesRunTimeRef.internalName, name, methodBType.descriptor, /*itf =*/ false)
  }

  private def calleeInMap(insn: MethodInsnNode, map: Map[InternalName, MethodNameAndType]): Boolean = map.get(insn.owner) match {
    case Some(MethodNameAndType(name, tp)) => insn.name == name && insn.desc == tp.descriptor
    case _ => false
  }

  def isJavaBox(insn: MethodInsnNode): Boolean = calleeInMap(insn, javaBoxMethods)
  def isJavaUnbox(insn: MethodInsnNode): Boolean = calleeInMap(insn, javaUnboxMethods)

  def isPredefAutoBox(insn: MethodInsnNode): Boolean = {
    insn.owner == PredefRef.internalName && (predefAutoBoxMethods.get(insn.name) match {
      case Some(tp) => insn.desc == tp.descriptor
      case _ => false
    })
  }

  def isPredefAutoUnbox(insn: MethodInsnNode): Boolean = {
    insn.owner == PredefRef.internalName && (predefAutoUnboxMethods.get(insn.name) match {
      case Some(tp) => insn.desc == tp.descriptor
      case _ => false
    })
  }

  def isRefCreate(insn: MethodInsnNode): Boolean = calleeInMap(insn, srRefCreateMethods)
  def isRefZero(insn: MethodInsnNode): Boolean = calleeInMap(insn, srRefZeroMethods)

  def runtimeRefClassBoxedType(refClass: InternalName): Type = Type.getArgumentTypes(srRefCreateMethods(refClass).methodType.descriptor)(0)

  def isSideEffectFreeCall(insn: MethodInsnNode): Boolean = {
    isScalaBox(insn) || isScalaUnbox(insn) ||
      isJavaBox(insn) || // not java unbox, it may NPE
      isSideEffectFreeConstructorCall(insn)
  }

  def isNonNullMethodInvocation(mi: MethodInsnNode): Boolean = {
    isJavaBox(mi) || isScalaBox(mi) || isPredefAutoBox(mi) || isRefCreate(mi) || isRefZero(mi)
  }

  def isModuleLoad(insn: AbstractInsnNode, moduleName: InternalName): Boolean = insn match {
    case fi: FieldInsnNode => fi.getOpcode == GETSTATIC && fi.owner == moduleName && fi.name == "MODULE$" && fi.desc == ("L" + moduleName + ";")
    case _ => false
  }

  def isPredefLoad(insn: AbstractInsnNode) = isModuleLoad(insn, PredefRef.internalName)

  def isPrimitiveBoxConstructor(insn: MethodInsnNode): Boolean = calleeInMap(insn, primitiveBoxConstructors)
  def isRuntimeRefConstructor(insn: MethodInsnNode): Boolean = calleeInMap(insn, srRefConstructors)
  def isTupleConstructor(insn: MethodInsnNode): Boolean = calleeInMap(insn, tupleClassConstructors)


  def isSideEffectFreeConstructorCall(insn: MethodInsnNode): Boolean = {
    insn.name == INSTANCE_CONSTRUCTOR_NAME && sideEffectFreeConstructors.get((insn.owner, insn.desc))
  }

  def isNewForSideEffectFreeConstructor(insn: AbstractInsnNode) = {
    insn.getOpcode == NEW && {
      val ti = insn.asInstanceOf[TypeInsnNode]
      classesOfSideEffectFreeConstructors.get.contains(ti.desc)
    }
  }

  def isBoxedUnit(insn: AbstractInsnNode) = {
    insn.getOpcode == GETSTATIC && {
      val fi = insn.asInstanceOf[FieldInsnNode]
      fi.owner == srBoxedUnitRef.internalName && fi.name == "UNIT" && fi.desc == srBoxedUnitRef.descriptor
    }
  }

  private class Collector extends NestedClassesCollector[ClassBType](nestedOnly = true) {
    def declaredNestedClasses(internalName: InternalName): List[ClassBType] =
      bTypesFromClassfile.classBTypeFromParsedClassfile(internalName).info.get.nestedClasses.force

    def getClassIfNested(internalName: InternalName): Option[ClassBType] = {
      val c = bTypesFromClassfile.classBTypeFromParsedClassfile(internalName)
      if (c.isNestedClass.get) Some(c) else None
    }

    def raiseError(msg: String, sig: String, e: Option[Throwable]): Unit = {
      // don't crash on invalid generic signatures
    }
  }
  /**
   * Visit the class node and collect all referenced nested classes.
   * @return (declaredInnerClasses, referredInnerClasses)
   */
  def collectNestedClasses(classNode: ClassNode): (List[ClassBType], List[ClassBType]) = {
    val c = new Collector
    c.visit(classNode)
    (c.declaredInnerClasses.toList, c.referredInnerClasses.toList)
  }

  /*
   * Populates the InnerClasses JVM attribute with `refedInnerClasses`. See also the doc on inner
   * classes in BTypes.scala.
   *
   * `refedInnerClasses` may contain duplicates, need not contain the enclosing inner classes of
   * each inner class it lists (those are looked up and included).
   *
   * This method serializes in the InnerClasses JVM attribute in an appropriate order, not
   * necessarily that given by `refedInnerClasses`.
   *
   * can-multi-thread
   */
  final def addInnerClasses(jclass: asm.tree.ClassNode, declaredInnerClasses: List[ClassBType], refedInnerClasses: List[ClassBType]) {
    // sorting ensures nested classes are listed after their enclosing class thus satisfying the Eclipse Java compiler
    val allNestedClasses = new mutable.TreeSet[ClassBType]()(Ordering.by(_.internalName))
    allNestedClasses ++= declaredInnerClasses
    refedInnerClasses.foreach(_.enclosingNestedClassesChain.get.foreach(allNestedClasses += _))

    for (nestedClass <- allNestedClasses) {
      // Extract the innerClassEntry - we know it exists, enclosingNestedClassesChain only returns nested classes.
      val Some(e) = nestedClass.innerClassAttributeEntry.get
      jclass.visitInnerClass(e.name, e.outerName, e.innerName, e.flags)
    }
  }

  def onIndyLambdaImplMethodIfPresent(hostClass: InternalName) (action : mutable.LinkedHashSet[asm.Handle] => Unit): Unit =
    indyLambdaImplMethods.get(hostClass) match {
      case null =>
      case xs => xs.synchronized(action(xs))
    }

  def onIndyLambdaImplMethod[T](hostClass: InternalName) (action: mutable.LinkedHashSet[asm.Handle] => T): T ={
    val methods = indyLambdaImplMethods.computeIfAbsent(hostClass, (_) => mutable.LinkedHashSet[asm.Handle]())

    methods.synchronized (action(methods))
  }

      /**
   * add methods
   * @return the added methods. Note the order is undefined
   */
  def addIndyLambdaImplMethod(hostClass: InternalName, handle: Seq[asm.Handle]): Seq[asm.Handle] = {
    if (handle.isEmpty) Nil else onIndyLambdaImplMethod(hostClass) {
      case set =>
        if (set.isEmpty) {
          set ++= handle
          handle
        } else {
          var added = List.empty[asm.Handle]
          handle foreach { h => if (set.add(h)) added ::= h }
          added
        }
    }
  }

  def addIndyLambdaImplMethod(hostClass: InternalName, handle: asm.Handle): Boolean = {
    onIndyLambdaImplMethod(hostClass) {
      _ add handle
    }
  }

  def removeIndyLambdaImplMethod(hostClass: InternalName, handle: Seq[asm.Handle]): Unit = {
    if (handle.nonEmpty)
      onIndyLambdaImplMethodIfPresent(hostClass) {
        _ --= handle
      }
  }

  def maxLocals(method: MethodNode): Int = {
    computeMaxLocalsMaxStack(method)
    method.maxLocals
  }

  def maxStack(method: MethodNode): Int = {
    computeMaxLocalsMaxStack(method)
    method.maxStack
  }

  /**
   * In order to run an Analyzer, the maxLocals / maxStack fields need to be available. The ASM
   * framework only computes these values during bytecode generation.
   *
   * NOTE 1: as explained in the `analysis` package object, the maxStack value used by the Analyzer
   * may be smaller than the correct maxStack value in the classfile (Analyzers only use a single
   * slot for long / double values). The maxStack computed here are correct for running an analyzer,
   * but not for writing in the classfile. We let the ClassWriter recompute max's.
   *
   * NOTE 2: the maxStack value computed here may be larger than the smallest correct value
   * that would allow running an analyzer, see `InstructionStackEffect.forAsmAnalysis` and
   * `InstructionStackEffect.maxStackGrowth`.
   *
   * NOTE 3: the implementation doesn't look at instructions that cannot be reached, it computes
   * the max local / stack size in the reachable code. These max's work just fine for running an
   * Analyzer: its implementation also skips over unreachable code in the same way.
   */
  def computeMaxLocalsMaxStack(method: MethodNode): Unit = {
    if (isAbstractMethod(method) || isNativeMethod(method)) {
      method.maxLocals = 0
      method.maxStack = 0
    } else if (!isMaxsComputed(method)) {
      val size = method.instructions.size

      var maxLocals = parametersSize(method)
      var maxStack = 0

      // queue of instruction indices where analysis should start
      var queue = new Array[Int](8)
      var top = -1
      def enq(i: Int): Unit = {
        if (top == queue.length - 1)
          queue = java.util.Arrays.copyOf(queue, queue.length * 2)
        top += 1
        queue(top) = i
      }
      def deq(): Int = {
        val r = queue(top)
        top -= 1
        r
      }

      // for each instruction in the queue, contains the stack height at this instruction.
      // once an instruction has been treated, contains -1 to prevent re-enqueuing
      val stackHeights = new Array[Int](size)

      def enqInsn(insn: AbstractInsnNode, height: Int): Unit = {
        enqInsnIndex(method.instructions.indexOf(insn), height)
      }

      def enqInsnIndex(insnIndex: Int, height: Int): Unit = {
        if (insnIndex < size && stackHeights(insnIndex) != -1) {
          stackHeights(insnIndex) = height
          enq(insnIndex)
        }
      }

      val tcbIt = method.tryCatchBlocks.iterator()
      while (tcbIt.hasNext) {
        val tcb = tcbIt.next()
        enqInsn(tcb.handler, 1)
        if (maxStack == 0) maxStack = 1
      }

      /* Subroutines are jumps, historically used for `finally` (https://www.artima.com/underthehood/finally.html)
       *   - JSR pushes the return address (next instruction) on the stack and jumps to a label
       *   - The subroutine typically saves the address to a local variable (ASTORE x)
       *   - The subroutine typically jumps back to the return address using `RET x`, where `x` is the local variable
       *
       * However, the JVM spec does not require subroutines to `RET x` to their caller, they could return back to an
       * outer subroutine caller (nested subroutines), or `RETURN`, or use a static jump. Static analysis of subroutines
       * is therefore complex (http://www21.in.tum.de/~kleing/papers/KleinW-TPHOLS03.pdf).
       *
       * The asm.Analyzer however makes the assumption that subroutines only occur in the shape emitted by early
       * javac, i.e., `RET` always returns to the next enclosing caller. So we do that as well.
       */

      enq(0)
      while (top != -1) {
        val insnIndex = deq()
        val insn = method.instructions.get(insnIndex)
        val initHeight = stackHeights(insnIndex)
        stackHeights(insnIndex) = -1 // prevent i from being enqueued again

        if (insn.getOpcode == -1) { // frames, labels, line numbers
          enqInsnIndex(insnIndex + 1, initHeight)
        } else {
          val stackGrowth = InstructionStackEffect.maxStackGrowth(insn)
          val heightAfter = initHeight + stackGrowth
          if (heightAfter > maxStack) maxStack = heightAfter

          // update maxLocals
          insn match {
            case v: VarInsnNode =>
              val longSize = if (isSize2LoadOrStore(v.getOpcode)) 1 else 0
              maxLocals = math.max(maxLocals, v.`var` + longSize + 1) // + 1 because local numbers are 0-based

            case i: IincInsnNode =>
              maxLocals = math.max(maxLocals, i.`var` + 1)

            case _ =>
          }

          insn match {
            case j: JumpInsnNode =>
              val opc = j.getOpcode
              if (opc == JSR) {
                val jsrTargetHeight = heightAfter + 1
                if (jsrTargetHeight > maxStack) maxStack = jsrTargetHeight
                enqInsn(j.label, jsrTargetHeight)
                enqInsnIndex(insnIndex + 1, heightAfter) // see subroutine shape assumption above
              } else {
                enqInsn(j.label, heightAfter)
                if (opc != GOTO) enqInsnIndex(insnIndex + 1, heightAfter) // jump is conditional, so the successor is also a possible control flow target
              }

            case l: LookupSwitchInsnNode =>
              var j = 0
              while (j < l.labels.size) {
                enqInsn(l.labels.get(j), heightAfter); j += 1
              }
              enqInsn(l.dflt, heightAfter)

            case t: TableSwitchInsnNode =>
              var j = 0
              while (j < t.labels.size) {
                enqInsn(t.labels.get(j), heightAfter); j += 1
              }
              enqInsn(t.dflt, heightAfter)

            case r: VarInsnNode if r.getOpcode == RET =>
              // the target is already enqueued, see subroutine shape assumption above

            case _ =>
              if (insn.getOpcode != ATHROW && !isReturn(insn))
                enqInsnIndex(insnIndex + 1, heightAfter)
          }
        }
      }

      method.maxLocals = maxLocals
      method.maxStack = maxStack

      setMaxsComputed(method)
    }
  }

  // not in `backendReporting` since there we don't have access to the `Callsite` class
  def optimizerWarningSiteString(cs: callGraph.Callsite): String =
    frontendAccess.backendReporting.siteString(cs.callsiteClass.internalName, cs.callsiteMethod.name)
}

object BackendUtils {
  /**
   * A pseudo-flag, added MethodNodes whose maxLocals / maxStack are computed. This allows invoking
   * `computeMaxLocalsMaxStack` whenever running an analyzer but performing the actual computation
   * only when necessary.
   *
   * The largest JVM flag (as of JDK 8) is ACC_MANDATED (0x8000), however the asm framework uses
   * the same trick and defines some pseudo flags
   *   - ACC_DEPRECATED = 0x20000
   *   - ACC_SYNTHETIC_ATTRIBUTE = 0x40000
   *   - ACC_CONSTRUCTOR = 0x80000
   *
   * I haven't seen the value picked here in use anywhere. We make sure to remove the flag when
   * it's no longer needed.
   */
  private val ACC_MAXS_COMPUTED = 0x1000000
  def isMaxsComputed(method: MethodNode) = (method.access & ACC_MAXS_COMPUTED) != 0
  def setMaxsComputed(method: MethodNode) = method.access |= ACC_MAXS_COMPUTED
  def clearMaxsComputed(method: MethodNode) = method.access &= ~ACC_MAXS_COMPUTED

  /**
   * A pseudo-flag indicating if a MethodNode's unreachable code has been eliminated.
   *
   * The ASM Analyzer class does not compute any frame information for unreachable instructions.
   * Transformations that use an analyzer (including inlining) therefore require unreachable code
   * to be eliminated.
   *
   * This flag allows running dead code elimination whenever an analyzer is used. If the method
   * is already optimized, DCE can return early.
   */
  private val ACC_DCE_DONE = 0x2000000
  def isDceDone(method: MethodNode) = (method.access & ACC_DCE_DONE) != 0
  def setDceDone(method: MethodNode) = method.access |= ACC_DCE_DONE
  def clearDceDone(method: MethodNode) = method.access &= ~ACC_DCE_DONE

  private val LABEL_REACHABLE_STATUS = 0x1000000
  private def isLabelFlagSet(l: LabelNode1, f: Int): Boolean = (l.flags & f) != 0

  private def setLabelFlag(l: LabelNode1, f: Int): Unit = {
    l.flags |= f
  }

  private def clearLabelFlag(l: LabelNode1, f: Int): Unit = {
    l.flags &= ~f
  }
  def isLabelReachable(label: LabelNode) = isLabelFlagSet(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)
  def setLabelReachable(label: LabelNode) = setLabelFlag(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)
  def clearLabelReachable(label: LabelNode) = clearLabelFlag(label.asInstanceOf[LabelNode1], LABEL_REACHABLE_STATUS)

  abstract class NestedClassesCollector[T](nestedOnly: Boolean) extends GenericSignatureVisitor(nestedOnly) {

    val declaredInnerClasses = mutable.Set.empty[T]
    val referredInnerClasses = mutable.Set.empty[T]

    def innerClasses: collection.Set[T] = declaredInnerClasses ++ referredInnerClasses
    def clear(): Unit = {
      declaredInnerClasses.clear()
      referredInnerClasses.clear()
    }

    def declaredNestedClasses(internalName: InternalName): List[T]

    def getClassIfNested(internalName: InternalName): Option[T]

    def visit(classNode: ClassNode): Unit = {
      visitInternalName(classNode.name)
      declaredInnerClasses ++= declaredNestedClasses(classNode.name)

      visitInternalName(classNode.superName)
      classNode.interfaces.asScala foreach visitInternalName
      visitInternalName(classNode.outerClass)

      visitAnnotations(classNode.visibleAnnotations)
      visitAnnotations(classNode.visibleTypeAnnotations)
      visitAnnotations(classNode.invisibleAnnotations)
      visitAnnotations(classNode.invisibleTypeAnnotations)

      visitClassSignature(classNode.signature)

      for (f <- classNode.fields.asScala) {
        visitDescriptor(f.desc)
        visitAnnotations(f.visibleAnnotations)
        visitAnnotations(f.visibleTypeAnnotations)
        visitAnnotations(f.invisibleAnnotations)
        visitAnnotations(f.invisibleTypeAnnotations)
        visitFieldSignature(f.signature)
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

        visitMethodSignature(m.signature)
      }
    }

    def visitInternalName(internalName: InternalName): Unit = if (internalName != null) {
      for (c <- getClassIfNested(internalName))
        if (!declaredInnerClasses.contains(c))
          referredInnerClasses += c
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
    // primitives and the brackets of array descriptors
    def visitDescriptor(desc: String): Unit = (desc.charAt(0): @switch) match {
      case '(' =>
        var i = 1
        while (i < desc.length) {
          if (desc.charAt(i) == 'L') {
            val start = i + 1 // skip the L
            var seenDollar = false
            while ({val ch = desc.charAt(i); seenDollar ||= (ch == '$'); ch != ';'}) i += 1
            if (seenDollar)
              visitInternalName(desc.substring(start, i))
          }
          // skips over '[', ')', primitives
          i += 1
        }

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
  }

  abstract class GenericSignatureVisitor(nestedOnly: Boolean) {
    def visitInternalName(internalName: InternalName): Unit

    def raiseError(msg: String, sig: String, e: Option[Throwable] = None): Unit

    def visitClassSignature(sig: String): Unit = if (sig != null) {
      val p = new Parser(sig, nestedOnly)
      p.safely { p.classSignature() }
    }

    def visitMethodSignature(sig: String): Unit = if (sig != null) {
      val p = new Parser(sig, nestedOnly)
      p.safely { p.methodSignature() }
    }

    def visitFieldSignature(sig: String): Unit = if (sig != null) {
      val p = new Parser(sig, nestedOnly)
      p.safely { p.fieldSignature() }
    }

    private final class Parser(sig: String, nestedOnly: Boolean) {

      private var index = 0
      private val end = sig.length

      private val Aborted: Throwable = new NoStackTrace { }
      private def abort(): Nothing = throw Aborted

      @inline def safely(f: => Unit): Unit = try f catch {
        case Aborted =>
        case NonFatal(e) => raiseError(s"Exception thrown during signature parsing", sig, Some(e))
      }

      private def current = {
        if (index >= end) {
          raiseError(s"Out of bounds, $index >= $end", sig)
          abort() // Don't continue, even if `notifyInvalidSignature` returns
        }
        sig.charAt(index)
      }

      private def accept(c: Char): Unit = {
        if (current != c) {
          raiseError(s"Expected $c at $index, found $current", sig)
          abort()
        }
        index += 1
      }

      private def skip(): Unit = { index += 1 }
      private def getCurrentAndSkip(): Char = { val c = current; skip(); c }

      private def skipUntil(isDelimiter: CharBooleanFunction): Unit = {
        while (!isDelimiter(current)) { index += 1 }
      }
      private def skipUntilDelimiter(delimiter: Char): Unit = {
        sig.indexOf(delimiter, index) match {
          case -1 =>
            raiseError(s"Out of bounds", sig)
            abort() // Don't continue, even if `notifyInvalidSignature` returns
          case i =>
            index = i
        }
      }

      private def appendUntil(builder: java.lang.StringBuilder, isDelimiter: CharBooleanFunction): Unit = {
        val start = index
        skipUntil(isDelimiter)
        builder.append(sig, start, index)
      }

      def isBaseType(c: Char): Boolean = c match {
        case 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => true
        case _ => false
      }

      private val isClassNameEnd: CharBooleanFunction = (c: Char) => c == '<' || c == '.' || c == ';'

      private def typeArguments(): Unit = if (current == '<') {
        skip()
        while (current != '>') current match {
          case '*' | '+' | '-' =>
            skip()
          case _ =>
            referenceTypeSignature()
        }
        accept('>')
      }

      @tailrec private def referenceTypeSignature(): Unit = getCurrentAndSkip() match {
        case 'L' =>
          var names: java.lang.StringBuilder = null

          val start = index
          var seenDollar = false
          while (!isClassNameEnd(current)) {
            seenDollar ||= current == '$'
            index += 1
          }
          if ((current == '.' || seenDollar) || !nestedOnly) {
            // OPT: avoid allocations when only a top-level class is encountered
            names = new java.lang.StringBuilder(32)
            names.append(sig, start, index)
            visitInternalName(names.toString)
          }
          typeArguments()

          while (current == '.') {
            skip()
            names.append('$')
            appendUntil(names, isClassNameEnd)
            visitInternalName(names.toString)
            typeArguments()
          }
          accept(';')

        case 'T' =>
          skipUntilDelimiter(';')
          skip()

        case '[' =>
          if (isBaseType(current)) skip()
          else referenceTypeSignature()
      }

      private def typeParameters(): Unit = if (current == '<') {
        skip()
        while (current != '>') {
          skipUntilDelimiter(':'); skip()
          val c = current
          // The ClassBound can be missing, but only if there's an InterfaceBound after.
          // This is an assumption that's not in the spec, see https://stackoverflow.com/q/44284928
          if (c != ':' && c != '>') { referenceTypeSignature() }
          while (current == ':') { skip(); referenceTypeSignature() }
        }
        accept('>')
      }

      def classSignature(): Unit = {
        typeParameters()
        while (index < end) referenceTypeSignature()
      }

      def methodSignature(): Unit = {
        typeParameters()

        accept('(')
        while (current != ')') {
          if (isBaseType(current)) skip()
          else referenceTypeSignature()
        }
        accept(')')

        if (current == 'V' || isBaseType(current)) skip()
        else referenceTypeSignature()

        while (index < end) {
          accept('^')
          referenceTypeSignature()
        }
      }

      def fieldSignature(): Unit = if (sig != null) safely {
        referenceTypeSignature()
      }
    }
  }
}

// For performance (`Char => Boolean` is not specialized)
private trait CharBooleanFunction { def apply(c: Char): Boolean }
