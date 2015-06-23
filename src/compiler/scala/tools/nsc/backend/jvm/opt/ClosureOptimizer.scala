/* NSC -- new Scala compiler
 * Copyright 2005-2015 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm
package opt

import scala.annotation.switch
import scala.reflect.internal.util.NoPosition
import scala.tools.asm.{Handle, Type, Opcodes}
import scala.tools.asm.tree._
import scala.tools.nsc.backend.jvm.BTypes.InternalName
import scala.tools.nsc.backend.jvm.analysis.ProdConsAnalyzer
import BytecodeUtils._
import BackendReporting._
import Opcodes._
import scala.tools.nsc.backend.jvm.opt.ByteCodeRepository.CompilationUnit
import scala.collection.convert.decorateAsScala._

class ClosureOptimizer[BT <: BTypes](val btypes: BT) {
  import btypes._
  import callGraph._

  def rewriteClosureApplyInvocations(): Unit = {
    closureInstantiations foreach {
      case (indy, (methodNode, ownerClass)) =>
        val warnings = rewriteClosureApplyInvocations(indy, methodNode, ownerClass)
        warnings.foreach(w => backendReporting.inlinerWarning(w.pos, w.toString))
    }
  }

  private val lambdaMetaFactoryInternalName: InternalName = "java/lang/invoke/LambdaMetafactory"

  private val metafactoryHandle = {
    val metafactoryMethodName: String = "metafactory"
    val metafactoryDesc: String       = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"
    new Handle(H_INVOKESTATIC, lambdaMetaFactoryInternalName, metafactoryMethodName, metafactoryDesc)
  }

  private val altMetafactoryHandle = {
    val altMetafactoryMethodName: String = "altMetafactory"
    val altMetafactoryDesc: String       = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;"
    new Handle(H_INVOKESTATIC, lambdaMetaFactoryInternalName, altMetafactoryMethodName, altMetafactoryDesc)
  }

  def isClosureInstantiation(indy: InvokeDynamicInsnNode): Boolean = {
    (indy.bsm == metafactoryHandle || indy.bsm == altMetafactoryHandle) &&
    {
      indy.bsmArgs match {
        case Array(samMethodType: Type, implMethod: Handle, instantiatedMethodType: Type, xs @ _*) =>
          // LambdaMetaFactory performs a number of automatic adaptations when invoking the lambda
          // implementation method (casting, boxing, unboxing, and primitive widening, see Javadoc).
          //
          // The closure optimizer supports only one of those adaptations: it will cast arguments
          // to the correct type when re-writing a closure call to the body method. Example:
          //
          //   val fun: String => String = l => l
          //   val l = List("")
          //   fun(l.head)
          //
          // The samMethodType of Function1 is `(Object)Object`, while the instantiatedMethodType
          // is `(String)String`. The return type of `List.head` is `Object`.
          //
          // The implMethod has the signature `C$anonfun(String)String`.
          //
          // At the closure callsite, we have an `INVOKEINTERFACE Function1.apply (Object)Object`,
          // so the object returned by `List.head` can be directly passed into the call (no cast).
          //
          // The closure object will cast the object to String before passing it to the implMethod.
          //
          // When re-writing the closure callsite to the implMethod, we have to insert a cast.
          //
          // The check below ensures that
          //   (1) the implMethod type has the expected singature (captured types plus argument types
          //       from instantiatedMethodType)
          //   (2) the receiver of the implMethod matches the first captured type
          //   (3) all parameters that are not the same in samMethodType and instantiatedMethodType
          //       are reference types, so that we can insert casts to perform the same adaptation
          //       that the closure object would.

          val isStatic = implMethod.getTag == H_INVOKESTATIC
          val indyParamTypes = Type.getArgumentTypes(indy.desc)
          val instantiatedMethodArgTypes = instantiatedMethodType.getArgumentTypes
          val expectedImplMethodType = {
            val paramTypes = (if (isStatic) indyParamTypes else indyParamTypes.tail) ++ instantiatedMethodArgTypes
            Type.getMethodType(instantiatedMethodType.getReturnType, paramTypes: _*)
          }

          {
            Type.getType(implMethod.getDesc) == expectedImplMethodType // (1)
          } && {
            isStatic || implMethod.getOwner == indyParamTypes(0).getInternalName // (2)
          } && {
            def isReference(t: Type) = t.getSort == Type.OBJECT || t.getSort == Type.ARRAY
            (samMethodType.getArgumentTypes, instantiatedMethodArgTypes).zipped forall {
              case (samArgType, instArgType) =>
                samArgType == instArgType || isReference(samArgType) && isReference(instArgType) // (3)
            }
          }

        case _ =>
          false
      }
    }
  }

  def isSamInvocation(invocation: MethodInsnNode, indy: InvokeDynamicInsnNode, prodCons: => ProdConsAnalyzer): Boolean = {
    if (invocation.getOpcode == INVOKESTATIC) false
    else {
      def closureIsReceiver = {
        val invocationFrame = prodCons.frameAt(invocation)
        val receiverSlot = {
          val numArgs = Type.getArgumentTypes(invocation.desc).length
          invocationFrame.stackTop - numArgs
        }
        val receiverProducers = prodCons.initialProducersForValueAt(invocation, receiverSlot)
        receiverProducers.size == 1 && receiverProducers.head == indy
      }

      invocation.name == indy.name && {
        val indySamMethodDesc = indy.bsmArgs(0).asInstanceOf[Type].getDescriptor // safe, checked in isClosureInstantiation
        indySamMethodDesc == invocation.desc
      } &&
      closureIsReceiver // most expensive check last
    }
  }

  /**
   * Stores the values captured by a closure creation into fresh local variables.
   * Returns the list of locals holding the captured values.
   */
  private def storeCaptures(indy: InvokeDynamicInsnNode, methodNode: MethodNode): LocalsList = {
    val capturedTypes = Type.getArgumentTypes(indy.desc)
    val firstCaptureLocal = methodNode.maxLocals

    // This could be optimized: in many cases the captured values are produced by LOAD instructions.
    // If the variable is not modified within the method, we could avoid introducing yet another
    // local. On the other hand, further optimizations (copy propagation, remove unused locals) will
    // clean it up.

    // Captured variables don't need to be cast when loaded at the callsite (castLoadTypes are None).
    // This is checked in `isClosureInstantiation`: the types of the captured variables in the indy
    // instruction match exactly the corresponding parameter types in the body method.
    val localsForCaptures = LocalsList.fromTypes(firstCaptureLocal, capturedTypes, castLoadTypes = _ => None)
    methodNode.maxLocals = firstCaptureLocal + localsForCaptures.size

    insertStoreOps(indy, methodNode, localsForCaptures)
    insertLoadOps(indy, methodNode, localsForCaptures)

    localsForCaptures
  }

  /**
   * Insert store operations in front of the `before` instruction to copy stack values into the
   * locals denoted by `localsList`.
   *
   * The lowest stack value is stored in the head of the locals list, so the last local is stored first.
   */
  private def insertStoreOps(before: AbstractInsnNode, methodNode: MethodNode, localsList: LocalsList) =
    insertLocalValueOps(before, methodNode, localsList, store = true)

  /**
   * Insert load operations in front of the `before` instruction to copy the local values denoted
   * by `localsList` onto the stack.
   *
   * The head of the locals list will be the lowest value on the stack, so the first local is loaded first.
   */
  private def insertLoadOps(before: AbstractInsnNode, methodNode: MethodNode, localsList: LocalsList) =
    insertLocalValueOps(before, methodNode, localsList, store = false)

  private def insertLocalValueOps(before: AbstractInsnNode, methodNode: MethodNode, localsList: LocalsList, store: Boolean): Unit = {
    // If `store` is true, the first instruction needs to store into the last local of the `localsList`.
    // Load instructions on the other hand are emitted in the order of the list.
    // To avoid reversing the list, we use `insert(previousInstr)` for stores and `insertBefore(before)` for loads.
    lazy val previous = before.getPrevious
    for (l <- localsList.locals) {
      val varOp = new VarInsnNode(if (store) l.storeOpcode else l.loadOpcode, l.local)
      if (store) methodNode.instructions.insert(previous, varOp)
      else methodNode.instructions.insertBefore(before, varOp)
      if (!store) for (castType <- l.castLoadedValue)
        methodNode.instructions.insert(varOp, new TypeInsnNode(CHECKCAST, castType.getInternalName))
    }
  }

  def rewriteClosureApplyInvocations(indy: InvokeDynamicInsnNode, methodNode: MethodNode, ownerClass: ClassBType): List[RewriteClosureApplyToClosureBodyFailed] = {
    val lambdaBodyHandle = indy.bsmArgs(1).asInstanceOf[Handle] // safe, checked in isClosureInstantiation

    // Kept as a lazy val to make sure the analysis is only computed if it's actually needed.
    // ProdCons is used to identify closure body invocations (see isSamInvocation), but only if the
    // callsite has the right name and signature. If the method has no invcation instruction with
    // the right name and signature, the analysis is not executed.
    lazy val prodCons = new ProdConsAnalyzer(methodNode, ownerClass.internalName)

    // First collect all callsites without modifying the instructions list yet.
    // Once we start modifying the instruction list, prodCons becomes unusable.

    // A list of callsites and stack heights. If the invocation cannot be rewritten, a warning
    // message is stored in the stack height value.
    val invocationsToRewrite: List[(MethodInsnNode, Either[RewriteClosureApplyToClosureBodyFailed, Int])] = methodNode.instructions.iterator.asScala.collect({
      case invocation: MethodInsnNode if isSamInvocation(invocation, indy, prodCons) =>
        val bodyAccessible: Either[OptimizerWarning, Boolean] = for {
          (bodyMethodNode, declClass) <- byteCodeRepository.methodNode(lambdaBodyHandle.getOwner, lambdaBodyHandle.getName, lambdaBodyHandle.getDesc): Either[OptimizerWarning, (MethodNode, InternalName)]
          isAccessible                <- inliner.memberIsAccessible(bodyMethodNode.access, classBTypeFromParsedClassfile(declClass), classBTypeFromParsedClassfile(lambdaBodyHandle.getOwner), ownerClass)
        } yield {
          isAccessible
        }

        def pos = callGraph.callsites.get(invocation).map(_.callsitePosition).getOrElse(NoPosition)
        val stackSize: Either[RewriteClosureApplyToClosureBodyFailed, Int] = bodyAccessible match {
          case Left(w)      => Left(RewriteClosureAccessCheckFailed(pos, w))
          case Right(false) => Left(RewriteClosureIllegalAccess(pos, ownerClass.internalName))
          case _            => Right(prodCons.frameAt(invocation).getStackSize)
        }

        (invocation, stackSize)
    }).toList

    if (invocationsToRewrite.isEmpty) Nil
    else {
      // lazy val to make sure locals for captures and arguments are only allocated if there's
      // effectively a callsite to rewrite.
      lazy val (localsForCapturedValues, argumentLocalsList) = {
        val captureLocals = storeCaptures(indy, methodNode)

        // allocate locals for storing the arguments of the closure apply callsites.
        // if there are multiple callsites, the same locals are re-used.
        val argTypes = indy.bsmArgs(0).asInstanceOf[Type].getArgumentTypes // safe, checked in isClosureInstantiation
        val firstArgLocal = methodNode.maxLocals

        // The comment in `isClosureInstantiation` explains why we have to introduce casts for
        // arguments that have different types in samMethodType and instantiatedMethodType.
        val castLoadTypes = {
          val instantiatedMethodType = indy.bsmArgs(2).asInstanceOf[Type]
          (argTypes, instantiatedMethodType.getArgumentTypes).zipped map {
            case (samArgType, instantiatedArgType) if samArgType != instantiatedArgType =>
              // isClosureInstantiation ensures that the two types are reference types, so we don't
              // end up casting primitive values.
              Some(instantiatedArgType)
            case _ =>
              None
          }
        }
        val argLocals = LocalsList.fromTypes(firstArgLocal, argTypes, castLoadTypes)
        methodNode.maxLocals = firstArgLocal + argLocals.size

        (captureLocals, argLocals)
      }

      val warnings = invocationsToRewrite flatMap {
        case (invocation, Left(warning)) => Some(warning)

        case (invocation, Right(stackHeight)) =>
          // store arguments
          insertStoreOps(invocation, methodNode, argumentLocalsList)

          // drop the closure from the stack
          methodNode.instructions.insertBefore(invocation, new InsnNode(POP))

          // load captured values and arguments
          insertLoadOps(invocation, methodNode, localsForCapturedValues)
          insertLoadOps(invocation, methodNode, argumentLocalsList)

          // update maxStack
          val capturesStackSize = localsForCapturedValues.size
          val invocationStackHeight = stackHeight + capturesStackSize - 1 // -1 because the closure is gone
          if (invocationStackHeight > methodNode.maxStack)
            methodNode.maxStack = invocationStackHeight

          // replace the callsite with a new call to the body method
          val bodyOpcode = (lambdaBodyHandle.getTag: @switch) match {
            case H_INVOKEVIRTUAL    => INVOKEVIRTUAL
            case H_INVOKESTATIC     => INVOKESTATIC
            case H_INVOKESPECIAL    => INVOKESPECIAL
            case H_INVOKEINTERFACE  => INVOKEINTERFACE
            case H_NEWINVOKESPECIAL =>
              val insns = methodNode.instructions
              insns.insertBefore(invocation, new TypeInsnNode(NEW, lambdaBodyHandle.getOwner))
              insns.insertBefore(invocation, new InsnNode(DUP))
              INVOKESPECIAL
          }
          val isInterface = bodyOpcode == INVOKEINTERFACE
          val bodyInvocation = new MethodInsnNode(bodyOpcode, lambdaBodyHandle.getOwner, lambdaBodyHandle.getName, lambdaBodyHandle.getDesc, isInterface)
          methodNode.instructions.insertBefore(invocation, bodyInvocation)
          methodNode.instructions.remove(invocation)

          // update the call graph
          val originalCallsite = callGraph.callsites.remove(invocation)

          // the method node is needed for building the call graph entry
          val bodyMethod = byteCodeRepository.methodNode(lambdaBodyHandle.getOwner, lambdaBodyHandle.getName, lambdaBodyHandle.getDesc)
          def bodyMethodIsBeingCompiled = byteCodeRepository.classNodeAndSource(lambdaBodyHandle.getOwner).map(_._2 == CompilationUnit).getOrElse(false)
          val bodyMethodCallsite = Callsite(
            callsiteInstruction = bodyInvocation,
            callsiteMethod = methodNode,
            callsiteClass = ownerClass,
            callee = bodyMethod.map({
              case (bodyMethodNode, bodyMethodDeclClass) => Callee(
                callee = bodyMethodNode,
                calleeDeclarationClass = classBTypeFromParsedClassfile(bodyMethodDeclClass),
                safeToInline = compilerSettings.YoptInlineGlobal || bodyMethodIsBeingCompiled,
                safeToRewrite = false, // the lambda body method is not a trait interface method
                annotatedInline = false,
                annotatedNoInline = false,
                calleeInfoWarning = None)
            }),
            argInfos = Nil,
            callsiteStackHeight = invocationStackHeight,
            receiverKnownNotNull = true, // see below (*)
            callsitePosition = originalCallsite.map(_.callsitePosition).getOrElse(NoPosition)
          )
          // (*) The documentation in class LambdaMetafactory says:
          //     "if implMethod corresponds to an instance method, the first capture argument
          //     (corresponding to the receiver) must be non-null"
          // Explanation: If the lambda body method is non-static, the receiver is a captured
          // value. It can only be captured within some instance method, so we know it's non-null.
          callGraph.callsites(bodyInvocation) = bodyMethodCallsite
          None
      }

      warnings.toList
    }
  }

  /**
   * A list of local variables. Each local stores information about its type, see class [[Local]].
   */
  case class LocalsList(locals: List[Local]) {
    val size = locals.iterator.map(_.size).sum
  }

  object LocalsList {
    /**
     * A list of local variables starting at `firstLocal` that can hold values of the types in the
     * `types` parameter.
     *
     * For example, `fromTypes(3, Array(Int, Long, String))` returns
     *   Local(3, intOpOffset)  ::
     *   Local(4, longOpOffset) ::  // note that this local occupies two slots, the next is at 6
     *   Local(6, refOpOffset)  ::
     *   Nil
     */
    def fromTypes(firstLocal: Int, types: Array[Type], castLoadTypes: Int => Option[Type]): LocalsList = {
      var sizeTwoOffset = 0
      val locals: List[Local] = types.indices.map(i => {
        // The ASM method `type.getOpcode` returns the opcode for operating on a value of `type`.
        val offset = types(i).getOpcode(ILOAD) - ILOAD
        val local = Local(firstLocal + i + sizeTwoOffset, offset, castLoadTypes(i))
        if (local.size == 2) sizeTwoOffset += 1
        local
      })(collection.breakOut)
      LocalsList(locals)
    }
  }

  /**
   * Stores a local varaible index the opcode offset required for operating on that variable.
   *
   * The xLOAD / xSTORE opcodes are in the following sequence: I, L, F, D, A, so the offset for
   * a local variable holding a reference (`A`) is 4. See also method `getOpcode` in [[scala.tools.asm.Type]].
   */
  case class Local(local: Int, opcodeOffset: Int, castLoadedValue: Option[Type]) {
    def size = if (loadOpcode == LLOAD || loadOpcode == DLOAD) 2  else 1

    def loadOpcode = ILOAD + opcodeOffset
    def storeOpcode = ISTORE + opcodeOffset
  }
}
