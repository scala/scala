/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.annotation._
import scala.collection.mutable

/**
  * This transformer is responsible for preparing Function nodes for runtime,
  * by translating to a tree that will be converted to an invokedynamic by the backend.
  *
  * The main assumption it makes is that a Function {args => body} has been turned into
  * {args => liftedBody()} where lifted body is a top level method that implements the body of the function.
  * Currently Uncurry is responsible for that transformation.
  *
  * From this shape of Function, Delambdafy will create:
  *
  * An application of the captured arguments to a fictional symbol representing the lambda factory.
  * This will be translated by the backed into an invokedynamic using a bootstrap method in JDK8's `LambdaMetaFactory`.
  * The captured arguments include `this` if `liftedBody` is unable to be made STATIC.
  */
abstract class Delambdafy extends Transform with TypingTransformers with ast.TreeDSL with TypeAdaptingTransformer {
  import global._
  import definitions._

  val analyzer: global.analyzer.type = global.analyzer

  /** the following two members override abstract members in Transform */
  val phaseName: String = "delambdafy"

  final case class LambdaMetaFactoryCapable(lambdaTarget: Symbol, arity: Int, functionalInterface: Symbol, sam: Symbol, bridges: List[Symbol], isSerializable: Boolean)

  /** Get the symbol of the target lifted lambda body method from a function. I.e. if
   *  the function is {args => anonfun(args)} then this method returns anonfun's symbol
   */
  private def targetMethod(fun: Function): Symbol = fun match {
    case Function(_, Apply(target, _)) => target.symbol
    case _ =>
      // any other shape of Function is unexpected at this point
      abort(s"could not understand function with tree $fun")
  }

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
    if (settings.Ydelambdafy.value == "method") new Phase(prev)
    else new SkipPhase(prev)
  }

  class SkipPhase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = ()
  }

  protected def newTransformer(unit: CompilationUnit): AstTransformer =
    new DelambdafyTransformer(unit)

  class DelambdafyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    // we need to know which methods refer to the 'this' reference so that we can determine which lambdas need access to it
    // TODO: this looks expensive, so I made it a lazy val. Can we make it more pay-as-you-go / optimize for common shapes?
    private[this] lazy val methodReferencesThis: collection.Set[Symbol] =
      (new ThisReferringMethodsTraverser).methodReferencesThisIn(unit.body)

    private def mkLambdaMetaFactoryCall(fun: Function, target: Symbol, functionalInterface: Symbol, samUserDefined: Symbol, userSamCls: Symbol, isSpecialized: Boolean): Tree = {
      /* user-defined SAM types should have gotten a class symbol made for them in `typer` */
      assert(isFunctionType(fun.tpe) || (samUserDefined.exists && userSamCls.isClass), s"$fun / ${fun.symbol} / ${fun.tpe}")

      val pos = fun.pos
      def isSelfParam(p: Symbol) = p.isSynthetic && p.name == nme.SELF
      val hasSelfParam = isSelfParam(target.firstParam)

      val allCapturedArgRefs = {
        // find which variables are free in the lambda because those are captures that need to be
        // passed into the constructor of the anonymous function class
        val captureArgs = FreeVarTraverser.freeVarsOf(fun).iterator.map(capture =>
          gen.mkAttributedRef(capture) setPos pos
        ).toList

        if (!hasSelfParam) captureArgs.filterNot(arg => isSelfParam(arg.symbol))
        else if (currentMethod.hasFlag(Flags.STATIC)) captureArgs
        else (gen.mkAttributedThis(fun.symbol.enclClass) setPos pos) :: captureArgs
      }

      // Create a symbol representing a fictional lambda factory method that accepts the captured
      // arguments and returns the SAM type.
      val msym = {
        val meth = currentOwner.newMethod(nme.ANON_FUN_NAME, pos, ARTIFACT)
        val capturedParams = meth.newSyntheticValueParams(allCapturedArgRefs.map(_.tpe))
        meth.setInfo(MethodType(capturedParams, fun.tpe))
      }

      // We then apply this symbol to the captures.
      val apply = localTyper.typedPos(pos)(Apply(Ident(msym), allCapturedArgRefs))

      // TODO: this is a bit gross
      val sam = samUserDefined orElse {
        if (isSpecialized) functionalInterface.info.decls.find(_.isDeferred).get
        else functionalInterface.info.member(nme.apply)
      }

      // no need for adaptation when the implemented sam is of a specialized built-in function type
      val lambdaTarget = if (isSpecialized) target else createBoxingBridgeMethodIfNeeded(fun, target, functionalInterface, sam)
      val isSerializable = samUserDefined == NoSymbol || functionalInterface.isNonBottomSubClass(definitions.SerializableClass)

      val samBridges = logResultIf[List[Symbol]](s"will add SAM bridges for $fun", _.nonEmpty) {
        userSamCls.fold[List[Symbol]](Nil) {
          _.info.findMember(sam.name, excludedFlags = 0L, requiredFlags = BRIDGE, stableOnly = false) match {
            case NoSymbol => Nil
            case bridges if bridges.isOverloaded => bridges.alternatives
            case bridge => bridge :: Nil
          }
        }
      }

      // The backend needs to know the target of the lambda and the functional interface in order
      // to emit the invokedynamic instruction. We pass this information as tree attachment.
      //
      // see https://docs.oracle.com/javase/8/docs/api/java/lang/invoke/LambdaMetafactory.html
      //   instantiatedMethodType is derived from lambdaTarget's signature
      //   samMethodType is derived from samOf(functionalInterface)'s signature
      apply.updateAttachment(LambdaMetaFactoryCapable(lambdaTarget, fun.vparams.length, functionalInterface, sam, samBridges, isSerializable))

      apply
    }


    private val boxingBridgeMethods = mutable.ArrayBuffer[Tree]()

    private def reboxValueClass(tp: Type) = tp match {
      case ErasedValueType(valueClazz, _) => TypeRef(NoPrefix, valueClazz, Nil)
      case _ => tp
    }

    // exclude primitives and value classes, which need special boxing
    private def isReferenceType(tp: Type) = !tp.isInstanceOf[ErasedValueType] && {
      val sym = tp.typeSymbol
      !(isPrimitiveValueClass(sym) || sym.isDerivedValueClass)
    }

    // determine which lambda target to use with java's LMF -- create a new one if scala-specific boxing is required
    def createBoxingBridgeMethodIfNeeded(fun: Function, target: Symbol, @unused functionalInterface: Symbol, sam: Symbol): Symbol = {
      val oldClass = fun.symbol.enclClass
      val pos = fun.pos

      // At erasure, there won't be any captured arguments (they are added in constructors)
      val functionParamTypes = exitingErasure(target.info.paramTypes)
      val functionResultType = exitingErasure(target.info.resultType)

      val samParamTypes = exitingErasure(sam.info.paramTypes)
      val samResultType = exitingErasure(sam.info.resultType)

      /* How to satisfy the linking invariants of https://docs.oracle.com/javase/8/docs/api/java/lang/invoke/LambdaMetafactory.html
       *
       * Given samMethodType: (U1..Un)Ru and function type T1,..., Tn => Rt (the target method created by uncurry)
       *
       * Do we need a bridge, or can we use the original lambda target for implMethod: (<captured args> A1..An)Ra
       * (We can ignore capture here.)
       *
       * If, for i=1..N:
       *  Ai =:= Ui || (Ai <:< Ui <:< AnyRef)
       *  Ru =:= void || (Ra =:= Ru || (Ra <:< AnyRef, Ru <:< AnyRef))
       *
       * We can use the target method as-is -- if not, we create a bridging one that uses the types closest
       * to the target method that still meet the above requirements.
       */
      val resTpOk = (
           samResultType =:= UnitTpe
        || functionResultType =:= samResultType
        || (isReferenceType(samResultType) && isReferenceType(functionResultType))) // per spec, no further correspondence required
      def paramTpsOk = samParamTypes.corresponds(functionParamTypes)((samParamTp, funParamTp) =>
          funParamTp =:= samParamTp ||
          (isReferenceType(funParamTp) && isReferenceType(samParamTp) && funParamTp <:< samParamTp))
      if (resTpOk && paramTpsOk) target
      else {
        // We have to construct a new lambda target that bridges to the one created by uncurry.
        // The bridge must satisfy the above invariants, while also minimizing adaptation on our end.
        // LMF will insert runtime casts according to the spec at the above link.

        // we use the more precise type between samParamTp and funParamTp to minimize boxing in the bridge method
        // we are constructing a method whose signature matches the sam's signature (because the original target did not)
        // whenever a type in the sam's signature is (erases to) a primitive type, we must pick the sam's version,
        // as we don't implement the logic regarding widening that's performed by LMF -- we require =:= for primitives
        //
        // We use the sam's type for the check whether we're dealing with a reference type, as it could be a generic type,
        // which means the function's parameter -- even if it expects a value class -- will need to be
        // boxed on the generic call to the sam method.

        val bridgeParamTypes = map2(samParamTypes, functionParamTypes) { (samParamTp, funParamTp) =>
          if (isReferenceType(samParamTp) && funParamTp <:< samParamTp) funParamTp
          else postErasure.elimErasedValueType(samParamTp)
        }

        val bridgeResultType =
          if (resTpOk && isReferenceType(samResultType) && functionResultType <:< samResultType) functionResultType
          else postErasure.elimErasedValueType(samResultType)

        val typeAdapter = new TypeAdapter { def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree) }
        import typeAdapter.{adaptToType, unboxValueClass}

        val targetParams = target.paramss.head
        val numCaptures  = targetParams.length - functionParamTypes.length
        val (targetCapturedParams, targetFunctionParams) = targetParams.splitAt(numCaptures)

        val methSym = oldClass.newMethod(target.name.append("$adapted").toTermName, target.pos, target.flags | FINAL | ARTIFACT | STATIC)
        val bridgeCapturedParams = targetCapturedParams.map(param => methSym.newSyntheticValueParam(param.tpe, param.name.toTermName))
        val bridgeFunctionParams =
          map2(targetFunctionParams, bridgeParamTypes)((param, tp) => methSym.newSyntheticValueParam(tp, param.name.toTermName))

        val bridgeParams = bridgeCapturedParams ::: bridgeFunctionParams

        methSym setInfo MethodType(bridgeParams, bridgeResultType)
        oldClass.info.decls enter methSym

        val forwarderCall = localTyper.typedPos(pos) {
          val capturedArgRefs = bridgeCapturedParams map gen.mkAttributedRef
          val functionArgRefs =
            map3(bridgeFunctionParams, functionParamTypes, targetParams.drop(numCaptures)) { (bridgeParam, functionParamTp, targetParam) =>
              val bridgeParamRef = gen.mkAttributedRef(bridgeParam)
              val targetParamTp  = targetParam.tpe

              // TODO: can we simplify this to something like `adaptToType(adaptToType(bridgeParamRef, functionParamTp), targetParamTp)`?
              val unboxed =
                functionParamTp match {
                  case ErasedValueType(clazz, underlying) =>
                    // when the original function expected an argument of value class type,
                    // the original target will expect the unboxed underlying value,
                    // whereas the bridge will receive the boxed value (since the sam's argument type did not match and we had to adapt)
                    localTyper.typed(unboxValueClass(bridgeParamRef, clazz, underlying), targetParamTp)
                  case _ => bridgeParamRef
                }

              adaptToType(unboxed, targetParamTp)
            }

          gen.mkMethodCall(Select(gen.mkAttributedThis(oldClass), target), capturedArgRefs ::: functionArgRefs)
        }
        val forwarderResultType =
          if (samResultType.isInstanceOf[ErasedValueType] && functionResultType.isInstanceOf[ErasedValueType]) bridgeResultType
          else functionResultType

        val bridge = postErasure.newTransformer(unit).transform(DefDef(methSym, List(bridgeParams.map(ValDef(_))),
          adaptToType(forwarderCall.setType(forwarderResultType), bridgeResultType))).asInstanceOf[DefDef]

        boxingBridgeMethods += bridge
        bridge.symbol
      }
    }


    private def transformFunction(originalFunction: Function): Tree = {
      val target = targetMethod(originalFunction)
      assert(target.hasFlag(Flags.STATIC), "static")
      target.setFlag(notPRIVATE)

      val funSym = originalFunction.tpe.typeSymbolDirect
      // The functional interface that can be used to adapt the lambda target method `target` to the given function type.
      val (functionalInterface, isSpecialized) =
        if (!isFunctionSymbol(funSym)) (funSym, false)
        else {
          val specializedName =
            specializeTypes.specializedFunctionName(funSym,
              exitingErasure(target.info.paramTypes).map(reboxValueClass) :+ reboxValueClass(exitingErasure(target.info.resultType))).toTypeName

          val isSpecialized = specializedName != funSym.name
          val functionalInterface =
            if (isSpecialized) {
              // Unfortunately we still need to use custom functional interfaces for specialized functions so that the
              // unboxed apply method is left abstract for us to implement.
              currentRun.runDefinitions.Scala_Java8_CompatPackage.info.decl(specializedName.prepend("J"))
            }
            else FunctionClass(originalFunction.vparams.length)

          (functionalInterface, isSpecialized)
        }

      val (sam, synthCls) = originalFunction.attachments.get[SAMFunction] match {
        case Some(SAMFunction(_, sam, synthCls)) => (sam,      synthCls)
        case None                                => (NoSymbol, NoSymbol)
      }
      mkLambdaMetaFactoryCall(originalFunction, target, functionalInterface, sam, synthCls, isSpecialized)
    }

    // here's the main entry point of the transform
    override def transform(tree: Tree): Tree = tree match {
      // the main thing we care about is lambdas
      case fun: Function =>
        super.transform(transformFunction(fun))
      case Template(_, _, _) =>
        def pretransform(tree: Tree): Tree = tree match {
          case dd: DefDef if dd.symbol.isDelambdafyTarget =>
            if (!dd.symbol.hasFlag(STATIC) && methodReferencesThis(dd.symbol)) {
              gen.mkStatic(dd, dd.symbol.name, sym => sym)
            } else {
              dd.symbol.setFlag(STATIC)
              dd
            }
          case t => t
        }
        try {
          // during this call boxingBridgeMethods will be populated from the Function case
          val Template(parents, self, body) = super.transform(deriveTemplate(tree)(_.mapConserve(pretransform))): @unchecked
          Template(parents, self, body ++ boxingBridgeMethods)
        } finally boxingBridgeMethods.clear()
      case dd: DefDef if dd.symbol.isLiftedMethod && !dd.symbol.isDelambdafyTarget =>
        // scala/bug#9390 emit lifted methods that don't require a `this` reference as STATIC
        // delambdafy targets are excluded as they are made static by `transformFunction`.
        // a synchronized method cannot be static (`methodReferencesThis` will not see the implicit this reference due to `this.synchronized`)
        if (!dd.symbol.hasFlag(STATIC) && !methodReferencesThis(dd.symbol)) {
          dd.symbol.setFlag(STATIC)
          dd.symbol.removeAttachment[mixer.NeedStaticImpl.type]
        }
        super.transform(tree)
      case Apply(fun, outer :: rest) if shouldElideOuterArg(fun.symbol, outer) =>
        val nullOuter = gen.mkZero(outer.tpe)
        treeCopy.Apply(tree, transform(fun), nullOuter :: transformTrees(rest))
      case _ => super.transform(tree)
    }
  } // DelambdafyTransformer

  private def shouldElideOuterArg(fun: Symbol, outerArg: Tree): Boolean =
    fun.isConstructor && treeInfo.isQualifierSafeToElide(outerArg) && fun.hasAttachment[OuterArgCanBeElided.type]

  // A traverser that finds symbols used but not defined in the given Tree
  // TODO freeVarTraverser in LambdaLift does a very similar task. With some
  // analysis this could probably be unified with it
  class FreeVarTraverser extends InternalTraverser {
    val freeVars = mutable.LinkedHashSet[Symbol]()
    val declared = mutable.LinkedHashSet[Symbol]()

    override def traverse(tree: Tree) = {
      tree match {
        case Function(args, _) =>
          args foreach {arg => declared += arg.symbol}
        case ValDef(_, _, _, _) =>
          declared += tree.symbol
        case _: Bind =>
          declared += tree.symbol
        case Ident(_) =>
          val sym = tree.symbol
          if ((sym != NoSymbol) && sym.isLocalToBlock && sym.isTerm && !sym.isMethod && !declared.contains(sym)) freeVars += sym
        case _ =>
      }
      tree.traverse(this)
    }
  }

  object FreeVarTraverser {
    def freeVarsOf(function: Function) = {
      val freeVarsTraverser = new FreeVarTraverser
      freeVarsTraverser.traverse(function)
      freeVarsTraverser.freeVars
    }
  }

  // finds all methods that reference 'this'
  class ThisReferringMethodsTraverser extends InternalTraverser {
    // the set of methods that refer to this
    private val thisReferringMethods = mutable.Set.empty[Symbol]

    // the set of lifted lambda body methods that each method refers to
    private val liftedMethodReferences = mutable.Map.empty[Symbol, mutable.Set[Symbol]]

    def methodReferencesThisIn(tree: Tree): collection.Set[Symbol] = {
      traverse(tree)
      liftedMethodReferences.keys foreach refersToThis

      thisReferringMethods
    }

    // recursively find methods that refer to 'this' directly or indirectly via references to other methods
    // for each method found add it to the referrers set
    private def refersToThis(symbol: Symbol): Boolean = {
      val seen = mutable.Set[Symbol]()
      def loop(symbol: Symbol): Boolean = {
        if (seen(symbol)) false
        else {
          seen += symbol
          (thisReferringMethods contains symbol) ||
            (liftedMethodReferences.contains(symbol) && liftedMethodReferences(symbol).exists(loop)) && {
              // add it early to memoize
              debuglog(s"$symbol indirectly refers to 'this'")
              thisReferringMethods += symbol
              true
            }
        }
      }
      loop(symbol)
    }

    private var currentMethod: Symbol = NoSymbol

    override def traverse(tree: Tree) = tree match {
      case _: DefDef if tree.symbol.hasFlag(SYNCHRONIZED) =>
        thisReferringMethods add tree.symbol
      case DefDef(_, _, _, _, _, _) if tree.symbol.isDelambdafyTarget || tree.symbol.isLiftedMethod =>
        // we don't expect defs within defs. At this phase trees should be very flat
        if (currentMethod.exists) devWarning("Found a def within a def at a phase where defs are expected to be flattened out.")
        currentMethod = tree.symbol
        tree.traverse(this)
        currentMethod = NoSymbol
      case fun@Function(_, _) =>
        // we don't drill into functions because at the beginning of this phase they will always refer to 'this'.
        // They'll be of the form {(args...) => this.anonfun(args...)}
        // but we do need to make note of the lifted body method in case it refers to 'this'
        if (currentMethod.exists) liftedMethodReferences.getOrElseUpdate(currentMethod, mutable.Set()) += targetMethod(fun)
      case Apply(sel @ Select(This(_), _), args) if sel.symbol.isLiftedMethod =>
        if (currentMethod.exists) liftedMethodReferences.getOrElseUpdate(currentMethod, mutable.Set()) += sel.symbol
        super.traverseTrees(args)
      case Apply(fun, outer :: rest) if shouldElideOuterArg(fun.symbol, outer) =>
        fun.traverse(this)
        super.traverseTrees(rest)
      case This(_) =>
        if (currentMethod.exists && tree.symbol == currentMethod.enclClass) {
          debuglog(s"$currentMethod directly refers to 'this'")
          thisReferringMethods add currentMethod
        }
      case _: ClassDef if !tree.symbol.isTopLevel =>
      case _: DefDef =>
      case _ =>
        tree.traverse(this)
    }
  }
}
