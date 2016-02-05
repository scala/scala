package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection._
import scala.collection.mutable.LinkedHashMap

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

  final case class LambdaMetaFactoryCapable(target: Symbol, arity: Int, functionalInterface: Symbol)

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
    if (settings.Ydelambdafy.value == "method") new Phase(prev)
    else new SkipPhase(prev)
  }

  class SkipPhase(prev: scala.tools.nsc.Phase) extends StdPhase(prev) {
    def apply(unit: global.CompilationUnit): Unit = ()
  }

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new DelambdafyTransformer(unit)

  class DelambdafyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) with TypeAdapter {
    private val lambdaClassDefs = new mutable.LinkedHashMap[Symbol, List[Tree]] withDefaultValue Nil

    val typer = localTyper

    // we need to know which methods refer to the 'this' reference so that we can determine which lambdas need access to it
    // TODO: this looks expensive, so I made it a lazy val. Can we make it more pay-as-you-go / optimize for common shapes?
    lazy val thisReferringMethods: Set[Symbol] = {
      val thisReferringMethodsTraverser = new ThisReferringMethodsTraverser()
      thisReferringMethodsTraverser traverse unit.body
      val methodReferringMap = thisReferringMethodsTraverser.liftedMethodReferences
      val referrers = thisReferringMethodsTraverser.thisReferringMethods
      // recursively find methods that refer to 'this' directly or indirectly via references to other methods
      // for each method found add it to the referrers set
      def refersToThis(symbol: Symbol): Boolean = {
        if (referrers contains symbol) true
        else if (methodReferringMap(symbol) exists refersToThis) {
          // add it early to memoize
          debuglog(s"$symbol indirectly refers to 'this'")
          referrers += symbol
          true
        } else false
      }
      methodReferringMap.keys foreach refersToThis
      referrers
    }

    private val boxingBridgeMethods = mutable.ArrayBuffer[Tree]()

    // here's the main entry point of the transform
    override def transform(tree: Tree): Tree = tree match {
      // the main thing we care about is lambdas
      case fun: Function => super.transform(transformFunction(fun))
      case Template(_, _, _) =>
        try {
          // during this call boxingBridgeMethods will be populated from the Function case
          val Template(parents, self, body) = super.transform(tree)
          Template(parents, self, body ++ boxingBridgeMethods)
        } finally boxingBridgeMethods.clear()
      case _ => super.transform(tree)
    }

    // this entry point is aimed at the statements in the compilation unit.
    // after working on the entire compilation until we'll have a set of
    // new class definitions to add to the top level
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      // Need to remove from the lambdaClassDefs map: there may be multiple PackageDef for the same
      // package when defining a package object. We only add the lambda class to one. See SI-9097.
      super.transformStats(stats, exprOwner) ++ lambdaClassDefs.remove(exprOwner).getOrElse(Nil)
    }

    def createBoxingBridgeMethod(oldClass: Symbol, target: Symbol, functionParamTypes: List[Type], functionResultType: Type, pos: Position): Symbol = {
      // Note: we bail out of this method and return EmptyTree if we find there is no adaptation required.
      // If we need to improve performance, we could check the types first before creating the
      // method and parameter symbols.
      val methSym = oldClass.newMethod(target.name.append("$adapted").toTermName, target.pos, target.flags | FINAL | ARTIFACT)
      var neededAdaptation = false
      def boxedType(tpe: Type): Type = {
        if (isPrimitiveValueClass(tpe.typeSymbol)) {neededAdaptation = true; ObjectTpe}
        else if (enteringErasure(tpe.typeSymbol.isDerivedValueClass)) {neededAdaptation = true; ObjectTpe}
        else tpe
      }
      val targetParams: List[Symbol] = target.paramss.head
      val numCaptures = targetParams.length - functionParamTypes.length
      val (targetCaptureParams, targetFunctionParams) = targetParams.splitAt(numCaptures)
      val bridgeParams: List[Symbol] =
        targetCaptureParams.map(param => methSym.newSyntheticValueParam(param.tpe, param.name.toTermName)) :::
        map2(targetFunctionParams, functionParamTypes)((param, tp) => methSym.newSyntheticValueParam(boxedType(tp), param.name.toTermName))

      val bridgeResultType: Type = {
        if (target.info.resultType == UnitTpe && functionResultType != UnitTpe) {
          neededAdaptation = true
          ObjectTpe
        } else
          boxedType(functionResultType)
      }
      val methodType = MethodType(bridgeParams, bridgeResultType)
      methSym setInfo methodType
      if (!neededAdaptation) target
      else {
        val bridgeParamTrees = bridgeParams.map(ValDef(_))

        oldClass.info.decls enter methSym

        val body = localTyper.typedPos(pos) {
          val newTarget = Select(gen.mkAttributedThis(oldClass), target)
          val args: List[Tree] = mapWithIndex(bridgeParams) { (param, i) =>
            if (i < numCaptures) {
              gen.mkAttributedRef(param)
            } else {
              val functionParam = functionParamTypes(i - numCaptures)
              val targetParam = targetParams(i)
              if (enteringErasure(functionParam.typeSymbol.isDerivedValueClass)) {
                val casted = cast(gen.mkAttributedRef(param), functionParam)
                val unboxed = unbox(casted, ErasedValueType(functionParam.typeSymbol, targetParam.tpe)).modifyType(postErasure.elimErasedValueType)
                unboxed
              } else adaptToType(gen.mkAttributedRef(param), targetParam.tpe)
            }
          }
          gen.mkMethodCall(newTarget, args)
        }
        val body1 = if (enteringErasure(functionResultType.typeSymbol.isDerivedValueClass))
          adaptToType(box(body.setType(ErasedValueType(functionResultType.typeSymbol, body.tpe)), "boxing lambda target"), bridgeResultType)
        else adaptToType(body, bridgeResultType)
        val methDef0 = DefDef(methSym, List(bridgeParamTrees), body1)
        val bridge = postErasure.newTransformer(unit).transform(methDef0).asInstanceOf[DefDef]
        boxingBridgeMethods += bridge
        bridge.symbol
      }
    }

    // turns a lambda into a new class def, a New expression instantiating that class
    private def transformFunction(originalFunction: Function): Tree = {
      val oldClass = originalFunction.symbol.enclClass
      val arity = originalFunction.vparams.length

      val target = targetMethod(originalFunction)
      target.makeNotPrivate(target.owner)

      if (!thisReferringMethods.contains(target))
        target setFlag STATIC

      val isStatic = target.hasFlag(STATIC)

      // Reconstruct the type of the function entering erasure.
      // We do this by taking the type after erasure, and re-boxing `ErasedValueType`.
      //
      // Unfortunately, the more obvious `enteringErasure(target.info)` doesn't work
      // as we would like, value classes in parameter position show up as the unboxed types.
      val (functionParamTypes, functionResultType) = exitingErasure {
        def boxed(tp: Type) = tp match {
          case ErasedValueType(valueClazz, _) => TypeRef(NoPrefix, valueClazz, Nil)
          case _ => tp
        }
        // We don't need to deeply map `boxedValueClassType` over the infos as `ErasedValueType`
        // will only appear directly as a parameter type in a method signature, as shown
        // https://gist.github.com/retronym/ba81dbd462282c504ff8
        val info = target.info
        val boxedParamTypes = info.paramTypes.takeRight(arity).map(boxed)
        (boxedParamTypes, boxed(info.resultType))
      }

      val functionType = definitions.functionType(functionParamTypes, functionResultType)
      val funSym = functionType.typeSymbol
      val specializedName = specializeTypes.specializedFunctionName(funSym, functionType.typeArgs).toTypeName
      val isSpecialized = specializedName != funSym.name

      // The functional interface that can be used to adapt the lambda target method `target` to the
      // given function type. Returns `NoSymbol` if the compiler settings are unsuitable.
      val functionalInterface =
        if (isSpecialized) currentRun.runDefinitions.Scala_Java8_CompatPackage.info.decl(specializedName.prepend("J"))
        else currentRun.runDefinitions.Scala_Java8_CompatPackage_JFunction(arity)

      assert(functionalInterface.exists)

      val lambdaTarget =
        if (isSpecialized) target
        else createBoxingBridgeMethod(oldClass, target, functionParamTypes, functionResultType, originalFunction.pos)

      // We then apply this symbol to the captures.
      val apply = {
        val allCaptureArgs: List[Tree] = {
          // find which variables are free in the lambda because those are captures that need to be
          // passed into the constructor of the anonymous function class
          val captures = FreeVarTraverser.freeVarsOf(originalFunction)
          val thisArg = if (isStatic) Nil else (gen.mkAttributedThis(oldClass) setPos originalFunction.pos) :: Nil
          val captureArgs = captures.iterator.map(capture => gen.mkAttributedRef(capture) setPos originalFunction.pos).toList
          thisArg ::: captureArgs
        }

        // Create a symbol representing a fictional lambda factory method that accepts the captured
        // arguments and returns a Function.
        val msym = {
          val meth = currentOwner.newMethod(nme.ANON_FUN_NAME, originalFunction.pos, ARTIFACT)
          val capturedParams = meth.newSyntheticValueParams(allCaptureArgs.map(_.tpe))
          meth.setInfo(MethodType(capturedParams, functionType))
        }

        localTyper.typedPos(originalFunction.pos)(Apply(Ident(msym), allCaptureArgs)).asInstanceOf[Apply]
      }

      // The backend needs to know the target of the lambda and the functional interface in order
      // to emit the invokedynamic instruction. We pass this information as tree attachment.
      apply.updateAttachment(LambdaMetaFactoryCapable(lambdaTarget, arity, functionalInterface))

      apply
    }
  } // DelambdafyTransformer

  /**
    * Get the symbol of the target lifted lambda body method from a function. I.e. if
    * the function is {args => anonfun(args)} then this method returns anonfun's symbol
    */
  private def targetMethod(fun: Function): Symbol = fun match {
    case Function(_, Apply(target, _)) => target.symbol
    case _ =>
      // any other shape of Function is unexpected at this point
      abort(s"could not understand function with tree $fun")
  }

  // A traverser that finds symbols used but not defined in the given Tree
  // TODO freeVarTraverser in LambdaLift does a very similar task. With some
  // analysis this could probably be unified with it
  class FreeVarTraverser extends Traverser {
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
      super.traverse(tree)
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
  class ThisReferringMethodsTraverser() extends Traverser {
    private var currentMethod: Symbol = NoSymbol
    // the set of methods that refer to this
    val thisReferringMethods = mutable.Set[Symbol]()
    // the set of lifted lambda body methods that each method refers to
    val liftedMethodReferences = mutable.Map[Symbol, Set[Symbol]]().withDefault(_ => mutable.Set())
    override def traverse(tree: Tree) = tree match {
      case DefDef(_, _, _, _, _, _) =>
        // we don't expect defs within defs. At this phase trees should be very flat
        if (currentMethod.exists) devWarning("Found a def within a def at a phase where defs are expected to be flattened out.")
        currentMethod = tree.symbol
        super.traverse(tree)
        currentMethod = NoSymbol
      case fun@Function(_, _) =>
        // we don't drill into functions because at the beginning of this phase they will always refer to 'this'.
        // They'll be of the form {(args...) => this.anonfun(args...)}
        // but we do need to make note of the lifted body method in case it refers to 'this'
        if (currentMethod.exists) liftedMethodReferences(currentMethod) += targetMethod(fun)
      case This(_) =>
        if (currentMethod.exists && tree.symbol == currentMethod.enclClass) {
          debuglog(s"$currentMethod directly refers to 'this'")
          thisReferringMethods add currentMethod
        }
      case _ =>
        super.traverse(tree)
    }
  }
}
