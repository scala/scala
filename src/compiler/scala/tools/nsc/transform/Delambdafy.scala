package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection._
import scala.language.postfixOps
import scala.reflect.internal.Symbols
import scala.collection.mutable.LinkedHashMap

/**
 * This transformer is responisble for turning lambdas into anonymous classes.
 * The main assumption it makes is that a lambda {args => body} has been turned into
 * {args => liftedBody()} where lifted body is a top level method that implements the body of the lambda.
 * Currently Uncurry is responsible for that transformation.
 *
 * From a lambda, Delambdafy will create
 * 1) a static forwarder at the top level of the class that contained the lambda
 * 2) a new top level class that
      a) has fields and a constructor taking the captured environment (including possbily the "this"
 *       reference)
 *    b) an apply method that calls the static forwarder
 *    c) if needed a bridge method for the apply method
 *  3) an instantiation of the newly created class which replaces the lambda
 *
 *  TODO the main work left to be done is to plug into specialization. Primarily that means choosing a
 * specialized FunctionN trait instead of the generic FunctionN trait as a parent and creating the
 * appropriately named applysp method
 */
abstract class Delambdafy extends Transform with TypingTransformers with ast.TreeDSL with TypeAdaptingTransformer {
  import global._
  import definitions._
  import CODE._

  val analyzer: global.analyzer.type = global.analyzer

  /** the following two members override abstract members in Transform */
  val phaseName: String = "delambdafy"

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

    val accessorMethods = mutable.ArrayBuffer[Tree]()

    // the result of the transformFunction method. A class definition for the lambda, an expression
    // insantiating the lambda class, and an accessor method for the lambda class to be able to
    // call the implementation
    case class TransformedFunction(lambdaClassDef: ClassDef, newExpr: Tree, accessorMethod: Tree)

    // here's the main entry point of the transform
    override def transform(tree: Tree): Tree = tree match {
      // the main thing we care about is lambdas
      case fun @ Function(_, _) =>
        // a lambda beccomes a new class, an instantiation expression, and an
        // accessor method
        val TransformedFunction(lambdaClassDef, newExpr, accessorMethod) = transformFunction(fun)
        // we'll add accessor methods to the current template later
        accessorMethods += accessorMethod
        val pkg = lambdaClassDef.symbol.owner

        // we'll add the lambda class to the package later
        lambdaClassDefs(pkg) = lambdaClassDef :: lambdaClassDefs(pkg)

        super.transform(newExpr)
      // when we encounter a template (basically the thing that holds body of a class/trait)
      // we need to updated it to include newly created accesor methods after transforming it
      case Template(_, _, _) =>
        try {
          // during this call accessorMethods will be populated from the Function case
          val Template(parents, self, body) = super.transform(tree)
          Template(parents, self, body ++ accessorMethods)
        } finally accessorMethods.clear()
      case _ => super.transform(tree)
    }

    // this entry point is aimed at the statements in the compilation unit.
    // after working on the entire compilation until we'll have a set of
    // new class definitions to add to the top level
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      super.transformStats(stats, exprOwner) ++ lambdaClassDefs(exprOwner)
    }

    private def optionSymbol(sym: Symbol): Option[Symbol] = if (sym.exists) Some(sym) else None

    // turns a lambda into a new class def, a New expression instantiating that class, and an
    // accessor method fo the body of the lambda
    private def transformFunction(originalFunction: Function): TransformedFunction = {
      val functionTpe = originalFunction.tpe
      val targs = functionTpe.typeArgs
      val formals :+ restpe = targs
      val oldClass = originalFunction.symbol.enclClass

      // find which variables are free in the lambda because those are captures that need to be
      // passed into the constructor of the anonymous function class
      val captures = FreeVarTraverser.freeVarsOf(originalFunction)

      /**
       * Creates the apply method for the anonymous subclass of FunctionN
       */
      def createAccessorMethod(thisProxy: Symbol, fun: Function): DefDef = {
        val target = targetMethod(fun)
        if (!thisProxy.exists) {
          target setFlag STATIC
        }
        val params = ((optionSymbol(thisProxy) map {proxy:Symbol => ValDef(proxy)}) ++ (target.paramss.flatten map ValDef.apply)).toList

        val methSym = oldClass.newMethod(unit.freshTermName(nme.accessor.toString() + "$"), target.pos, FINAL | BRIDGE | SYNTHETIC | PROTECTED | STATIC)

        val paramSyms = params map {param => methSym.newSyntheticValueParam(param.symbol.tpe, param.name) }

        params zip paramSyms foreach { case (valdef, sym) => valdef.symbol = sym }
        params foreach (_.symbol.owner = methSym)

        val methodType = MethodType(paramSyms, restpe)
        methSym setInfo methodType

        oldClass.info.decls enter methSym

        val body = localTyper.typed {
          val newTarget = Select(if (thisProxy.exists) gen.mkAttributedRef(paramSyms(0)) else gen.mkAttributedThis(oldClass), target)
          val newParams = paramSyms drop (if (thisProxy.exists) 1 else 0) map Ident
          Apply(newTarget, newParams)
        } setPos fun.pos
        val methDef = DefDef(methSym, List(params), body)

        // Have to repack the type to avoid mismatches when existentials
        // appear in the result - see SI-4869.
        // TODO probably don't need packedType
        methDef.tpt setType localTyper.packedType(body, methSym)
        methDef
      }

      /**
       * Creates the apply method for the anonymous subclass of FunctionN
       */
      def createApplyMethod(newClass: Symbol, fun: Function, accessor: DefDef, thisProxy: Symbol): DefDef = {
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

        val body = localTyper typed Apply(Select(gen.mkAttributedThis(oldClass), accessor.symbol), (optionSymbol(thisProxy) map {tp => Select(gen.mkAttributedThis(newClass), tp)}).toList ++ oldParams)
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
      def makeAnonymousClass = {
        val parents = addSerializable(abstractFunctionErasedType)
        val funOwner = originalFunction.symbol.owner

        // TODO harmonize the naming of delamdafy anon-fun classes with those spun up by Uncurry
        //      - make `anonClass.isAnonymousClass` true.
        //      - use `newAnonymousClassSymbol` or push the required variations into a similar factory method
        //      - reinstate the assertion in `Erasure.resolveAnonymousBridgeClash`
        val suffix = "$lambda$" + (
          if (funOwner.isPrimaryConstructor) ""
          else "$" + funOwner.name + "$"
        )
        val name = unit.freshTypeName(s"${oldClass.name.decode}$suffix")

        val anonClass = pkg newClassSymbol(name, originalFunction.pos, FINAL | SYNTHETIC) addAnnotation SerialVersionUIDAnnotation
        anonClass setInfo ClassInfoType(parents, newScope, anonClass)

        val captureProxies2 = new LinkedHashMap[Symbol, TermSymbol]
        captures foreach {capture =>
          val sym = anonClass.newVariable(capture.name.toTermName, capture.pos, SYNTHETIC)
          sym setInfo capture.info
          captureProxies2 += ((capture, sym))
        }

      // the Optional proxy that will hold a reference to the 'this'
      // object used by the lambda, if any. NoSymbol if there is no this proxy
      val thisProxy = {
        val target = targetMethod(originalFunction)
        if (thisReferringMethods contains target) {
          val sym = anonClass.newVariable(nme.FAKE_LOCAL_THIS, originalFunction.pos, SYNTHETIC)
          sym.info = oldClass.tpe
          sym
        } else NoSymbol
      }

      val decapturify = new DeCapturifyTransformer(captureProxies2, unit, oldClass, anonClass, originalFunction.symbol.pos, thisProxy)

      val accessorMethod = createAccessorMethod(thisProxy, originalFunction)

      val decapturedFunction = decapturify.transform(originalFunction).asInstanceOf[Function]

      val members = (optionSymbol(thisProxy).toList ++ (captureProxies2 map (_._2))) map {member =>
        anonClass.info.decls enter member
        ValDef(member, gen.mkZero(member.tpe)) setPos decapturedFunction.pos
      }

      // constructor
      val constr = createConstructor(anonClass, members)

      // apply method with same arguments and return type as original lambda.
      val applyMethodDef = createApplyMethod(anonClass, decapturedFunction, accessorMethod, thisProxy)

      val bridgeMethod = createBridgeMethod(anonClass, originalFunction, applyMethodDef)

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
        (localTyper.typedPos(decapturedFunction.pos)(ClassDef(anonClass, body)).asInstanceOf[ClassDef], thisProxy, accessorMethod)
      }

      val (anonymousClassDef, thisProxy, accessorMethod) = makeAnonymousClass

      pkg.info.decls enter anonymousClassDef.symbol

      val thisArg = optionSymbol(thisProxy) map (_ => gen.mkAttributedThis(oldClass) setPos originalFunction.pos)
      val captureArgs = captures map (capture => Ident(capture) setPos originalFunction.pos)

      val newStat =
          Typed(New(anonymousClassDef.symbol, (thisArg.toList ++ captureArgs): _*), TypeTree(abstractFunctionErasedType))

      val typedNewStat = localTyper.typedPos(originalFunction.pos)(newStat)

      TransformedFunction(anonymousClassDef, typedNewStat, accessorMethod)
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
        val postErasedTree = trans.atOwner(currentOwner)(trans.transform(adaptedTree)) // SI-8017 elimnates ErasedValueTypes
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
   * Get the symbol of the target lifted lambad body method from a function. I.e. if
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
}
