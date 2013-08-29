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

  /** the following two members override abstract members in Transform */
  val phaseName: String = "delambdafy"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new DelambdafyTransformer(unit)

  class DelambdafyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) with TypeAdapter {
    private val lambdaClassDefs = new mutable.LinkedHashMap[Symbol, List[Tree]] withDefaultValue Nil
    
    // we need to know which methods refer to the 'this' reference so that we can determine 
    // which lambdas need access to it
    val thisReferringMethods = {
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
    
    // here's the main entry point of the transform
    override def transform(tree: Tree): Tree = tree match {
      // the main thing we care about is lambdas
      case fun @ Function(_, _) =>
        // a lambda beccomes a new class, an instantiation expression, and an
        // accessor method
        val (lambdaClassDef, newExpr, accessorMethod) = transformFunction(fun)
        // we'll add accessor methods to the current template later
        accessorMethods += accessorMethod
        val pkg = lambdaClassDef.symbol.owner

        // we'll add the lambda class to the package later
        lambdaClassDefs(pkg) = lambdaClassDef :: lambdaClassDefs(pkg)
        
        val transformedNewExpr = super.transform(newExpr)
        transformedNewExpr
      // when we encounter a template (basically the thing that holds body of a class/trait)
      // we need to updated it to include newly created accesor methods after transforming it
      case Template(_, _, _) =>
        // during this call accessorMethods will be populated from the Function case
        val Template(parents, self, body) = super.transform(tree)
        val newTemplate = Template(parents, self, body ++ accessorMethods)
        accessorMethods.clear()
        newTemplate
      case _ => super.transform(tree)
    }
   
    // this entry point is aimed at the statements in the compilation unit.
    // after working on the entire compilation until we'll have a set of
    // new class definitions to add to the top level
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      super.transformStats(stats, exprOwner) ++ lambdaClassDefs(exprOwner)
    }  

    // turns a lambda into a new class def, a New expression instantiating that class, and an
    // accessor method fo the body of the lambda
    private def transformFunction(originalFunction: Function): (ClassDef, Tree, Tree) = {
      val functionTpe = originalFunction.tpe 
      val targs = functionTpe.typeArgs
      val (formals, restpe) = (targs.init, targs.last)
      val oldClass = originalFunction.symbol.enclClass
      
      // find which variables are free in the lambda because those are captures that need to be
      // passed into the constructor of the anonymous function class
      val freeVarsTraverser = new FreeVarTraverser
      freeVarsTraverser.traverse(originalFunction)
      val captures = freeVarsTraverser.freeVars

      /**
       * Creates the apply method for the anonymous subclass of FunctionN
       */
      def createAccessorMethod(thisProxy: Option[Symbol], fun: Function): DefDef = {
        val target = targetMethod(fun)
        if (!thisProxy.isDefined) {
          target setFlag STATIC
        }
        val params = ((thisProxy map {proxy:Symbol => ValDef(proxy)}) ++ (target.paramss.flatten map ValDef)).toList

        val methSym = oldClass.newMethod(unit.freshTermName(nme.accessor.toString()), target.pos, FINAL | BRIDGE | SYNTHETIC | PROTECTED | STATIC)

        val paramSyms = params map {param => methSym.newSyntheticValueParam(param.symbol.tpe, param.name) }
          
        params zip paramSyms foreach { case (valdef, sym) => valdef.symbol = sym }
        params foreach (_.symbol.owner = methSym)
        
        val methodType = MethodType(paramSyms, restpe)
        methSym setInfo methodType
      
        oldClass.info.decls enter methSym

        val body = localTyper.typed {
          val newTarget = if (thisProxy.isDefined) Select(Ident(paramSyms(0)), target) else Select(gen.mkAttributedThis(oldClass), target)
          val newParams = paramSyms drop (if (thisProxy.isDefined) 1 else 0) map Ident
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
      def createApplyMethod(newClass: Symbol, fun: Function, accessor: DefDef, thisProxy: Option[Symbol]): DefDef = {
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
        
        val body = localTyper typed Apply(Select(gen.mkAttributedThis(oldClass), accessor.symbol), (thisProxy map {tp => Select(gen.mkAttributedThis(newClass), tp)}).toList ++ oldParams)
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
        constrSym setInfo constrType

        newClass.info.decls enter constrSym
        
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
      val abstractFunctionErasedType = {
        val originalFunctionTpe = originalFunction.tpe 
        val functionArity = originalFunctionTpe.typeArgs.init.length
        val abstractFunctionClass = AbstractFunctionClass(functionArity)
        val tpe = abstractFunctionClass.tpe
        tpe
      }

      // anonymous subclass of FunctionN with an apply method
      def makeAnonymousClass = {
        val parents = addSerializable(abstractFunctionErasedType)
        val funOwner = originalFunction.symbol.owner
        val ownerSuffix = if (funOwner.isPrimaryConstructor) ""
        else "$" + funOwner.name
        val name = unit.freshTypeName(oldClass.name.decode + "$lambda"+ ownerSuffix + "$")
        
        val anonClass = pkg newClassSymbol(name, originalFunction.pos, FINAL | SYNTHETIC) addAnnotation serialVersionUIDAnnotation
        anonClass setInfo ClassInfoType(parents, newScope, anonClass)
        
        val captureProxies2 = new LinkedHashMap[Symbol, TermSymbol]
	      captureProxies2 ++= (captures map {capture => 
	      val sym = anonClass.newVariable(capture.name.toTermName, capture.pos, SYNTHETIC)
	      sym setInfo capture.info
	      (capture, sym)
	    })
	
      // the Optional proxy that will hold a reference to the 'this'
      // object used by the lambda, if any
	    val thisProxy = {
        val target = targetMethod(originalFunction)
	      if (thisReferringMethods contains target) {
          val sym = anonClass.newVariable(nme.FAKE_LOCAL_THIS, originalFunction.pos, SYNTHETIC)
          sym.info = oldClass.tpe
          Some(sym)
	      } else None
	    }
        
	    val decapturify = new DeCapturifyTransformer(captureProxies2, unit, oldClass, anonClass, originalFunction.symbol.pos, thisProxy)
	
      val accessorMethod = createAccessorMethod(thisProxy, originalFunction)
       
	    val decapturedFunction = decapturify.transform(originalFunction).asInstanceOf[Function]
          
      val members = (thisProxy.toList ++ (captureProxies2 map (_._2))) map {member =>
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
          
      def clashError(bm: Symbol) = {
        unit.error(
          applyMethodDef.symbol.pos,
            sm"""bridge generated for member ${fulldef(applyMethodDef.symbol)}
                |which overrides ${fulldef(getMember(abstractFunctionErasedType.typeSymbol, nme.apply))}
                |clashes with definition of the member itself;
                |both have erased type ${exitingPostErasure(bm.tpe)}""")
        }
        
        bridgeMethod foreach {bm =>
          if (bm.symbol.tpe =:= applyMethodDef.symbol.tpe)
            clashError(bm.symbol)
        }
        
        val template = Template(anonClass.info.parents map TypeTree, emptyValDef, members ++ List(constr, applyMethodDef) ++ bridgeMethod) setPos decapturedFunction.pos
        
        // TODO if member fields are private this complains that they're not accessible
        (localTyper.typed(ClassDef(anonClass, template) setPos decapturedFunction.pos).asInstanceOf[ClassDef], thisProxy, accessorMethod)
      }
      
      val (anonymousClassDef, thisProxy, accessorMethod) = makeAnonymousClass
            
      pkg.info.decls enter anonymousClassDef.symbol
      
      val thisArg = thisProxy.toList map (_ => gen.mkAttributedThis(oldClass) setPos originalFunction.pos)
      val captureArgs = captures map (capture => Ident(capture) setPos originalFunction.pos)

      val newStat = 
          Typed(New(anonymousClassDef.symbol, (thisArg ++ captureArgs): _*) setPos originalFunction.pos, TypeTree(abstractFunctionErasedType))

      val typedNewStat = 
          localTyper.typed(newStat)

      (anonymousClassDef, typedNewStat, accessorMethod)
    }
    
    /**
     * Creates a bridge method if needed. The bridge method forwards from apply(x1: Object, x2: Object...xn: Object): Object to
     * apply(x1: T1, x2: T2...xn: Tn): T0 using type adaptation on each input and output. The only time a bridge isn't needed
     * is when the original lambda is already type Object, Object, Object... => Object
     */
    def createBridgeMethod(newClass:Symbol, originalFunction: Function, applyMethod: DefDef): Option[DefDef] = {
      val methSym = newClass.newMethod(nme.apply, applyMethod.pos, FINAL | SYNTHETIC | BRIDGE)
      val originalParams = applyMethod.vparamss(0)
      val bridgeParams = originalParams map { originalParam =>
      val bridgeSym = methSym.newSyntheticValueParam(ObjectClass.tpe, originalParam.name)
        bridgeSym.owner = methSym
        ValDef(bridgeSym)
      }

      val bridgeSyms = bridgeParams map (_.symbol)

      val methodType = MethodType(bridgeSyms, ObjectClass.tpe)
      methSym setInfo methodType

      def adapt(tree: Tree, expectedTpe: Type): (Boolean, Tree) = {
        if (tree.tpe =:= expectedTpe) (false, tree)
        else (true, adaptToType(tree, expectedTpe))
      }

      enteringPhase(currentRun.posterasurePhase) {
	      val liftedBodyDefTpe: MethodType = {
	        val liftedBodySymbol = {
	          val Apply(method, _) = originalFunction.body
	          method.symbol
	        }
	        liftedBodySymbol.info.asInstanceOf[MethodType]
	      }
        val (paramNeedsAdaptation, adaptedParams) = (bridgeSyms zip liftedBodyDefTpe.params map {case (bridgeSym, param) => adapt(Ident(bridgeSym) setType bridgeSym.tpe, param.tpe)}).unzip
        val body = Apply(gen.mkAttributedSelect(gen.mkAttributedThis(newClass), applyMethod.symbol), adaptedParams) setType applyMethod.symbol.tpe.resultType
        val (needsReturnAdaptation, adaptedBody) = adapt(typer.typed(body), ObjectClass.tpe)
        val needsBridge = (paramNeedsAdaptation contains true) || needsReturnAdaptation
        if (needsBridge) {
          val methDef = DefDef(methSym, List(bridgeParams), adaptedBody)
          newClass.info.decls enter methSym
          Some((localTyper typed methDef).asInstanceOf[DefDef])
        } else None
      }
    }
  } // DelambdafyTransformer

  // A traverser that finds symbols used but not defined in the given Tree
  class FreeVarTraverser extends Traverser {
    val freeVars = mutable.LinkedHashSet[Symbol]()
    val declared = mutable.LinkedHashSet[Symbol]()
    
    override def traverse(tree: Tree) = {
      tree match {
        case Function(args, body) => 
          args foreach {arg => declared += arg.symbol}
        case ValDef(_, _, _, _) =>
          declared += tree.symbol
        case _: Bind =>
          declared += tree.symbol
        case Ident(_) =>
          val sym = tree.symbol
          if ((sym != NoSymbol) && sym.isLocal && sym.isTerm && !sym.isMethod && !declared.contains(sym)) freeVars += sym
        case _ =>
      }
      super.traverse(tree)
    }
  }
  
  // A transformer that converts specified captured symbols into other symbols
  class DeCapturifyTransformer(captureProxies: Map[Symbol, TermSymbol], unit: CompilationUnit, oldClass: Symbol, newClass:Symbol, pos: Position, thisProxy: Option[Symbol]) extends TypingTransformer(unit) {
    override def transform(tree: Tree) = tree match {
      case tree@This(encl) if tree.symbol == oldClass && thisProxy.isDefined => 
        localTyper typed Select(gen.mkAttributedThis(newClass), thisProxy.get)
      case Ident(name) if (captureProxies contains tree.symbol) => 
        localTyper typed Select(gen.mkAttributedThis(newClass), captureProxies(tree.symbol))
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
      sys.error(s"could not understand function with tree $fun")      
  }

  private lazy val serialVersionUIDAnnotation =
    AnnotationInfo(SerialVersionUIDAttr.tpe, List(Literal(Constant(0))), List())
  
  // finds all methods that reference 'this'
  class ThisReferringMethodsTraverser() extends Traverser {
    private var currentMethod: Option[Symbol] = None
    // the set of methods that refer to this
    val thisReferringMethods = mutable.Set[Symbol]()
    // the set of lifted lambda body methods that each method refers to
    val liftedMethodReferences = mutable.Map[Symbol, Set[Symbol]]().withDefault(_ => mutable.Set())
    override def traverse(tree: Tree) = tree match {
      case DefDef(_, _, _, _, _, _) =>
        // we don't expect defs within defs. At this phase trees should be very flat
        assert(currentMethod.isEmpty)
        currentMethod = Some(tree.symbol)
        super.traverse(tree)
        currentMethod = None
      case fun@Function(_, _) => 
        // we don't drill into functions because at the beginning of this phase they will always refer to 'this'. 
        // They'll be of the form {(args...) => this.anonfun(args...)}
        // but we do need to make note of the lifted body method in case it refers to 'this'
        currentMethod foreach {m => liftedMethodReferences(m) += targetMethod(fun)}
      case This(_) =>
        currentMethod foreach {method =>
          if (tree.symbol == method.enclClass) {
            debuglog(s"$method directly refers to 'this'")
            thisReferringMethods add method
          }
        }
      case _ =>
        super.traverse(tree)
    }
  }
}
