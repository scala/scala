package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection._
import scala.language.postfixOps
import scala.reflect.internal.Symbols
import scala.collection.mutable.LinkedHashMap

/**
 * This transformer is responsible for preparing lambdas for runtime, by either translating to anonymous classes
 * or to a tree that will be convereted to invokedynamic by the JVM 1.8+ backend.
 *
 * The main assumption it makes is that a lambda {args => body} has been turned into
 * {args => liftedBody()} where lifted body is a top level method that implements the body of the lambda.
 * Currently Uncurry is responsible for that transformation.
 *
 * From a lambda, Delambdafy will create:
 *
 * Under -target:jvm-1.7 and below:
 *
 * 1) a new top level class that
      a) has fields and a constructor taking the captured environment (including possibly the "this"
 *       reference)
 *    b) an apply method that calls the target method
 *    c) if needed a bridge method for the apply method
 * 2) an instantiation of the newly created class which replaces the lambda
 *
 * Under -target:jvm-1.8 with GenBCode:
 *
 * 1) An application of the captured arguments to a fictional symbol representing the lambda factory.
 *    This will be translated by the backed into an invokedynamic using a bootstrap method in JDK8's `LambdaMetaFactory`.
 *    The captured arguments include `this` if `liftedBody` is unable to be made STATIC.
 */
abstract class Delambdafy extends Transform with TypingTransformers with ast.TreeDSL with TypeAdaptingTransformer {
  import global._
  import definitions._

  val analyzer: global.analyzer.type = global.analyzer

  /** the following two members override abstract members in Transform */
  val phaseName: String = "delambdafy"

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

    // we need to know which methods refer to the 'this' reference so that we can determine
    // which lambdas need access to it
    val thisReferringMethods: Set[Symbol] = {
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

    // the result of the transformFunction method.
    sealed abstract class TransformedFunction
    // A class definition for the lambda, an expression instantiating the lambda class
    case class DelambdafyAnonClass(lambdaClassDef: ClassDef, newExpr: Tree) extends TransformedFunction
    case class InvokeDynamicLambda(tree: Apply) extends TransformedFunction

    private val boxingBridgeMethods = mutable.ArrayBuffer[Tree]()

    // here's the main entry point of the transform
    override def transform(tree: Tree): Tree = tree match {
      // the main thing we care about is lambdas
      case fun @ Function(_, _) =>
        transformFunction(fun) match {
          case DelambdafyAnonClass(lambdaClassDef, newExpr) =>
            // a lambda becomes a new class, an instantiation expression
            val pkg = lambdaClassDef.symbol.owner

            // we'll add the lambda class to the package later
            lambdaClassDefs(pkg) = lambdaClassDef :: lambdaClassDefs(pkg)

            super.transform(newExpr)
          case InvokeDynamicLambda(apply) =>
            // ... or an invokedynamic call
            super.transform(apply)
        }
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

    private def optionSymbol(sym: Symbol): Option[Symbol] = if (sym.exists) Some(sym) else None

    // turns a lambda into a new class def, a New expression instantiating that class
    private def transformFunction(originalFunction: Function): TransformedFunction = {
      val functionTpe = originalFunction.tpe
      val targs = functionTpe.typeArgs
      val formals :+ restpe = targs
      val oldClass = originalFunction.symbol.enclClass

      // find which variables are free in the lambda because those are captures that need to be
      // passed into the constructor of the anonymous function class
      val captures = FreeVarTraverser.freeVarsOf(originalFunction)

      val target = targetMethod(originalFunction)
      target.makeNotPrivate(target.owner)
      if (!thisReferringMethods.contains(target))
        target setFlag STATIC

      val isStatic = target.hasFlag(STATIC)

      def createBoxingBridgeMethod(functionParamTypes: List[Type], functionResultType: Type): Tree = {
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
        if (!neededAdaptation)
          EmptyTree
        else {
          val bridgeParamTrees = bridgeParams.map(ValDef(_))

          oldClass.info.decls enter methSym

          val body = localTyper.typedPos(originalFunction.pos) {
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
          postErasure.newTransformer(unit).transform(methDef0).asInstanceOf[DefDef]
        }
      }
      /**
       * Creates the apply method for the anonymous subclass of FunctionN
       */
      def createApplyMethod(newClass: Symbol, fun: Function, thisProxy: Symbol): DefDef = {
        val methSym = newClass.newMethod(nme.apply, fun.pos, FINAL | SYNTHETIC)
        val params = fun.vparams map (_.duplicate)

        val paramSyms = map2(formals, params) {
          (tp, vparam) => methSym.newSyntheticValueParam(tp, vparam.name)
        }
        params zip paramSyms foreach { case (valdef, sym) => valdef.symbol = sym }
        params foreach (_.symbol.owner = methSym)

        val methodType = MethodType(paramSyms, restpe)
        methSym setInfo methodType

        newClass.info.decls enter methSym

        val Apply(_, oldParams) = fun.body
        val qual = if (thisProxy.exists)
          Select(gen.mkAttributedThis(newClass), thisProxy)
        else
          gen.mkAttributedThis(oldClass) // sort of a lie, EmptyTree.<static method> would be more honest, but the backend chokes on that.

        val body = localTyper typed Apply(Select(qual, target), oldParams)
        body.substituteSymbols(fun.vparams map (_.symbol), params map (_.symbol))
        body changeOwner (fun.symbol -> methSym)

        val methDef = DefDef(methSym, List(params), body)

        // Have to repack the type to avoid mismatches when existentials
        // appear in the result - see SI-4869.
        // TODO probably don't need packedType
        methDef.tpt setType localTyper.packedType(body, methSym)
        methDef
      }

      /**
       * Creates the constructor on the newly created class. It will handle
       * initialization of members that represent the captured environment
       */
      def createConstructor(newClass: Symbol, members: List[ValDef]): DefDef = {
        val constrSym = newClass.newConstructor(originalFunction.pos, SYNTHETIC)

        val (paramSymbols, params, assigns) = (members map {member =>
          val paramSymbol = newClass.newVariable(member.symbol.name.toTermName, newClass.pos, 0)
          paramSymbol.setInfo(member.symbol.info)
          val paramVal = ValDef(paramSymbol)
          val paramIdent = Ident(paramSymbol)
          val assign = Assign(Select(gen.mkAttributedThis(newClass), member.symbol), paramIdent)

          (paramSymbol, paramVal, assign)
        }).unzip3

        val constrType = MethodType(paramSymbols, newClass.thisType)
        constrSym setInfoAndEnter constrType

        val body =
          Block(
            List(
              Apply(Select(Super(gen.mkAttributedThis(newClass), tpnme.EMPTY) setPos newClass.pos, nme.CONSTRUCTOR) setPos newClass.pos, Nil) setPos newClass.pos
            ) ++ assigns,
            Literal(Constant(())): Tree
          ) setPos newClass.pos

        (localTyper typed DefDef(constrSym, List(params), body) setPos newClass.pos).asInstanceOf[DefDef]
      }

      val pkg = oldClass.owner

      // Parent for anonymous class def
      val abstractFunctionErasedType = AbstractFunctionClass(formals.length).tpe

      // anonymous subclass of FunctionN with an apply method
      def makeAnonymousClass: ClassDef = {
        val parents = addSerializable(abstractFunctionErasedType)
        val funOwner = originalFunction.symbol.owner

        // TODO harmonize the naming of delambdafy anon-fun classes with those spun up by Uncurry
        //      - make `anonClass.isAnonymousClass` true.
        //      - use `newAnonymousClassSymbol` or push the required variations into a similar factory method
        //      - reinstate the assertion in `Erasure.resolveAnonymousBridgeClash`
        val suffix = nme.DELAMBDAFY_LAMBDA_CLASS_NAME + "$" + (
          if (funOwner.isPrimaryConstructor) ""
          else "$" + funOwner.name + "$"
        )
        val oldClassPart = oldClass.name.decode
        // make sure the class name doesn't contain $anon, otherwise isAnonymousClass/Function may be true
        val name = unit.freshTypeName(s"$oldClassPart$suffix".replace("$anon", "$nestedInAnon"))

        val lambdaClass = pkg newClassSymbol(name, originalFunction.pos, FINAL | SYNTHETIC) addAnnotation SerialVersionUIDAnnotation
        lambdaClass.associatedFile = unit.source.file
        // make sure currentRun.compiles(lambdaClass) is true (AddInterfaces does the same for trait impl classes)
        currentRun.symSource(lambdaClass) = funOwner.sourceFile
        lambdaClass setInfo ClassInfoType(parents, newScope, lambdaClass)
        assert(!lambdaClass.isAnonymousClass && !lambdaClass.isAnonymousFunction, "anonymous class name: "+ lambdaClass.name)
        assert(lambdaClass.isDelambdafyFunction, "not lambda class name: " + lambdaClass.name)

        val captureProxies2 = new LinkedHashMap[Symbol, TermSymbol]
        captures foreach {capture =>
          val sym = lambdaClass.newVariable(unit.freshTermName(capture.name.toString + "$"), capture.pos, SYNTHETIC)
          sym setInfo capture.info
          captureProxies2 += ((capture, sym))
        }

        // the Optional proxy that will hold a reference to the 'this'
        // object used by the lambda, if any. NoSymbol if there is no this proxy
        val thisProxy = {
          if (isStatic)
            NoSymbol
          else {
            val sym = lambdaClass.newVariable(nme.FAKE_LOCAL_THIS, originalFunction.pos, SYNTHETIC)
            sym.setInfo(oldClass.tpe)
          }
        }

        val decapturify = new DeCapturifyTransformer(captureProxies2, unit, oldClass, lambdaClass, originalFunction.symbol.pos, thisProxy)

        val decapturedFunction = decapturify.transform(originalFunction).asInstanceOf[Function]

        val members = (optionSymbol(thisProxy).toList ++ (captureProxies2 map (_._2))) map {member =>
          lambdaClass.info.decls enter member
          ValDef(member, gen.mkZero(member.tpe)) setPos decapturedFunction.pos
        }

        // constructor
        val constr = createConstructor(lambdaClass, members)

        // apply method with same arguments and return type as original lambda.
        val applyMethodDef = createApplyMethod(lambdaClass, decapturedFunction, thisProxy)

        val bridgeMethod = createBridgeMethod(lambdaClass, originalFunction, applyMethodDef)

        def fulldef(sym: Symbol) =
          if (sym == NoSymbol) sym.toString
          else s"$sym: ${sym.tpe} in ${sym.owner}"

        bridgeMethod foreach (bm =>
          // TODO SI-6260 maybe just create the apply method with the signature (Object => Object) in all cases
          //      rather than the method+bridge pair.
          if (bm.symbol.tpe =:= applyMethodDef.symbol.tpe)
            erasure.resolveAnonymousBridgeClash(applyMethodDef.symbol, bm.symbol)
        )

        val body = members ++ List(constr, applyMethodDef) ++ bridgeMethod

        // TODO if member fields are private this complains that they're not accessible
        localTyper.typedPos(decapturedFunction.pos)(ClassDef(lambdaClass, body)).asInstanceOf[ClassDef]
      }

      val allCaptureArgs: List[Tree] = {
        val thisArg = if (isStatic) Nil else (gen.mkAttributedThis(oldClass) setPos originalFunction.pos) :: Nil
        val captureArgs = captures.iterator.map(capture => gen.mkAttributedRef(capture) setPos originalFunction.pos).toList
        thisArg ::: captureArgs
      }

      val arity = originalFunction.vparams.length

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

      val (functionalInterface, isSpecialized) = java8CompatFunctionalInterface(target, functionType)
      if (functionalInterface.exists) {
        // Create a symbol representing a fictional lambda factory method that accepts the captured
        // arguments and returns a Function.
        val msym = currentOwner.newMethod(nme.ANON_FUN_NAME, originalFunction.pos, ARTIFACT)
        val argTypes: List[Type] = allCaptureArgs.map(_.tpe)
        val params = msym.newSyntheticValueParams(argTypes)
        msym.setInfo(MethodType(params, functionType))
        val arity = originalFunction.vparams.length

        val lambdaTarget =
          if (isSpecialized)
            target
          else {
            createBoxingBridgeMethod(functionParamTypes, functionResultType) match {
              case EmptyTree =>
                target
              case bridge =>
                boxingBridgeMethods += bridge
                bridge.symbol
            }
          }

        // We then apply this symbol to the captures.
        val apply = localTyper.typedPos(originalFunction.pos)(Apply(Ident(msym), allCaptureArgs)).asInstanceOf[Apply]

        // The backend needs to know the target of the lambda and the functional interface in order
        // to emit the invokedynamic instruction. We pass this information as tree attachment.
        apply.updateAttachment(LambdaMetaFactoryCapable(lambdaTarget, arity, functionalInterface))
        InvokeDynamicLambda(apply)
      } else {
        val anonymousClassDef = makeAnonymousClass
        pkg.info.decls enter anonymousClassDef.symbol
        val newStat = Typed(New(anonymousClassDef.symbol, allCaptureArgs: _*), TypeTree(abstractFunctionErasedType))
        val typedNewStat = localTyper.typedPos(originalFunction.pos)(newStat)
        DelambdafyAnonClass(anonymousClassDef, typedNewStat)
      }
    }

    /**
     * Creates a bridge method if needed. The bridge method forwards from apply(x1: Object, x2: Object...xn: Object): Object to
     * apply(x1: T1, x2: T2...xn: Tn): T0 using type adaptation on each input and output. The only time a bridge isn't needed
     * is when the original lambda is already erased to type Object, Object, Object... => Object
     */
    def createBridgeMethod(newClass:Symbol, originalFunction: Function, applyMethod: DefDef): Option[DefDef] = {
      val bridgeMethSym = newClass.newMethod(nme.apply, applyMethod.pos, FINAL | SYNTHETIC | BRIDGE)
      val originalParams = applyMethod.vparamss(0)
      val bridgeParams = originalParams map { originalParam =>
        val bridgeSym = bridgeMethSym.newSyntheticValueParam(ObjectTpe, originalParam.name)
        ValDef(bridgeSym)
      }

      val bridgeSyms = bridgeParams map (_.symbol)

      val methodType = MethodType(bridgeSyms, ObjectTpe)
      bridgeMethSym setInfo methodType

      def adapt(tree: Tree, expectedTpe: Type): (Boolean, Tree) = {
        if (tree.tpe =:= expectedTpe) (false, tree)
        else (true, adaptToType(tree, expectedTpe))
      }

      def adaptAndPostErase(tree: Tree, pt: Type): (Boolean, Tree) = {
        val (needsAdapt, adaptedTree) = adapt(tree, pt)
        val trans = postErasure.newTransformer(unit)
        val postErasedTree = trans.atOwner(currentOwner)(trans.transform(adaptedTree)) // SI-8017 eliminates ErasedValueTypes
        (needsAdapt, postErasedTree)
      }

      enteringPhase(currentRun.posterasurePhase) {
        // e.g, in:
        //   class C(val a: Int) extends AnyVal; (x: Int) => new C(x)
        //
        // This type is:
        //    (x: Int)ErasedValueType(class C, Int)
        val liftedBodyDefTpe: MethodType = {
          val liftedBodySymbol = {
            val Apply(method, _) = originalFunction.body
            method.symbol
          }
          liftedBodySymbol.info.asInstanceOf[MethodType]
        }
        val (paramNeedsAdaptation, adaptedParams) = (bridgeSyms zip liftedBodyDefTpe.params map {case (bridgeSym, param) => adapt(Ident(bridgeSym) setType bridgeSym.tpe, param.tpe)}).unzip
        // SI-8017 Before, this code used `applyMethod.symbol.info.resultType`.
        //         But that symbol doesn't have a type history that goes back before `delambdafy`,
        //         so we just see a plain `Int`, rather than `ErasedValueType(C, Int)`.
        //         This triggered primitive boxing, rather than value class boxing.
        val resTp = liftedBodyDefTpe.finalResultType
        val body = Apply(gen.mkAttributedSelect(gen.mkAttributedThis(newClass), applyMethod.symbol), adaptedParams) setType resTp
        val (needsReturnAdaptation, adaptedBody) = adaptAndPostErase(body, ObjectTpe)

        val needsBridge = (paramNeedsAdaptation contains true) || needsReturnAdaptation
        if (needsBridge) {
          val methDef = DefDef(bridgeMethSym, List(bridgeParams), adaptedBody)
          newClass.info.decls enter bridgeMethSym
          Some((localTyper typed methDef).asInstanceOf[DefDef])
        } else None
      }
    }
  } // DelambdafyTransformer

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

  // A transformer that converts specified captured symbols into other symbols
  // TODO this transform could look more like ThisSubstituter and TreeSymSubstituter. It's not clear that it needs that level of sophistication since the types
  // at this point are always very simple flattened/erased types, but it would probably be more robust if it tried to take more complicated types into account
  class DeCapturifyTransformer(captureProxies: Map[Symbol, TermSymbol], unit: CompilationUnit, oldClass: Symbol, newClass:Symbol, pos: Position, thisProxy: Symbol) extends TypingTransformer(unit) {
    override def transform(tree: Tree) = tree match {
      case tree@This(encl) if tree.symbol == oldClass && thisProxy.exists =>
        gen mkAttributedSelect (gen mkAttributedThis newClass, thisProxy)
      case Ident(name) if (captureProxies contains tree.symbol) =>
        gen mkAttributedSelect (gen mkAttributedThis newClass, captureProxies(tree.symbol))
      case _ => super.transform(tree)
    }
  }

  /**
   * Get the symbol of the target lifted lambda body method from a function. I.e. if
   * the function is {args => anonfun(args)} then this method returns anonfun's symbol
   */
  private def targetMethod(fun: Function): Symbol = fun match {
    case Function(_, Apply(target, _)) =>
      target.symbol
    case _ =>
      // any other shape of Function is unexpected at this point
      abort(s"could not understand function with tree $fun")
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

  final case class LambdaMetaFactoryCapable(target: Symbol, arity: Int, functionalInterface: Symbol)

  // The functional interface that can be used to adapt the lambda target method `target` to the
  // given function type. Returns `NoSymbol` if the compiler settings are unsuitable.
  private def java8CompatFunctionalInterface(target: Symbol, functionType: Type): (Symbol, Boolean) = {
    val canUseLambdaMetafactory: Boolean = {
      val isTarget18 = settings.target.value.contains("jvm-1.8")
      settings.isBCodeActive && isTarget18
    }

    val sym = functionType.typeSymbol
    val pack = currentRun.runDefinitions.Scala_Java8_CompatPackage
    val name1 = specializeTypes.specializedFunctionName(sym, functionType.typeArgs)
    val paramTps :+ restpe = functionType.typeArgs
    val arity = paramTps.length
    val isSpecialized = name1.toTypeName != sym.name
    val functionalInterface = if (!isSpecialized) {
      currentRun.runDefinitions.Scala_Java8_CompatPackage_JFunction(arity)
    } else {
      pack.info.decl(name1.toTypeName.prepend("J"))
    }
    (if (canUseLambdaMetafactory) functionalInterface else NoSymbol, isSpecialized)
  }
}
