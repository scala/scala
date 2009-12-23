/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author
 */
// $Id$

package scala.tools.nsc
package transform

import symtab.Flags._
import scala.collection.mutable.{HashMap, HashSet}
import scala.tools.nsc.util.Position

/*<export>*/
/** - uncurry all symbol and tree types (@see UnCurryPhase)
 *  - for every curried parameter list:  (ps_1) ... (ps_n) ==> (ps_1, ..., ps_n)
 *  - for every curried application: f(args_1)...(args_n) ==> f(args_1, ..., args_n)
 *  - for every type application: f[Ts] ==> f[Ts]() unless followed by parameters
 *  - for every use of a parameterless function: f ==> f()  and  q.f ==> q.f()
 *  - for every def-parameter:  x: => T ==> x: () => T
 *  - for every use of a def-parameter: x ==> x.apply()
 *  - for every argument to a def parameter `x: => T':
 *      if argument is not a reference to a def parameter:
 *        convert argument `e' to (expansion of) `() => e'
 *  - for every repeated Scala parameter `x: T*' --> x: Seq[T].
 *  - for every repeated Java parameter `x: T...' --> x: Array[T], except:
 *    if T is an unbounded abstract type, replace --> x: Array[Object]
 *  - for every argument list that corresponds to a repeated Scala parameter
 *       (a_1, ..., a_n) => (Seq(a_1, ..., a_n))
 *  - for every argument list that corresponds to a repeated Java parameter
 *       (a_1, ..., a_n) => (Array(a_1, ..., a_n))
 *  - for every argument list that is an escaped sequence
 *       (a_1:_*) => (a_1) (possibly converted to sequence or array, as needed)
 *  - convert implicit method types to method types
 *  - convert non-trivial catches in try statements to matches
 *  - convert non-local returns to throws with enclosing try statements.
 */
/*</export>*/
abstract class UnCurry extends InfoTransform with TypingTransformers {
  import global._                  // the global environment
  import definitions._             // standard classes and methods

  val phaseName: String = "uncurry"

  def newTransformer(unit: CompilationUnit): Transformer = new UnCurryTransformer(unit)
  override def changesBaseClasses = false

// ------ Type transformation --------------------------------------------------------

// uncurry and uncurryType expand type aliases
  private def expandAlias(tp: Type): Type = if (!tp.isHigherKinded) tp.normalize else tp

  private def isUnboundedGeneric(tp: Type) = tp match {
    case t @ TypeRef(_, sym, _) => sym.isAbstractType && !(t <:< AnyRefClass.tpe)
    case _ => false
  }

  private val uncurry: TypeMap = new TypeMap {
    def apply(tp0: Type): Type = {
      val tp = expandAlias(tp0)
      tp match {
        case MethodType(params, MethodType(params1, restpe)) =>
          apply(MethodType(params ::: params1, restpe))
        case MethodType(formals, ExistentialType(tparams, restpe @ MethodType(_, _))) =>
          assert(false, "unexpected curried method types with intervening exitential")
          tp0
        case mt: ImplicitMethodType =>
          apply(MethodType(mt.params, mt.resultType))
        case PolyType(List(), restpe) => // nullary method type
          apply(MethodType(List(), restpe))
        case PolyType(tparams, restpe) => // polymorphic nullary method type, since it didn't occur in a higher-kinded position
          PolyType(tparams, apply(MethodType(List(), restpe)))
        case TypeRef(pre, ByNameParamClass, List(arg)) =>
          apply(functionType(List(), arg))
        case TypeRef(pre, RepeatedParamClass, args) =>
          apply(appliedType(SeqClass.typeConstructor, args))
        case TypeRef(pre, JavaRepeatedParamClass, args) =>
          apply(arrayType(
            if (isUnboundedGeneric(args.head)) ObjectClass.tpe else args.head))
        case _ =>
          expandAlias(mapOver(tp))
      }
    }

//@M TODO: better fix for the gross hack that conflates polymorphic nullary method types with type functions
// `[tpars] tref` (PolyType(tpars, tref)) could uncurry to either:
//   - `[tpars]() tref` (PolyType(tpars, MethodType(List(), tref))
//         a nullary method types uncurry to a method with an empty argument list
//   - `[tpars] tref`   (PolyType(tpars, tref))
//         a proper type function -- see mapOverArgs: can only occur in args of TypeRef (right?))
// the issue comes up when a partial type application gets normalised to a polytype, like `[A] Function1[X, A]`
// should not apply the uncurry transform to such a type
// see #2594 for an example

    // decide whether PolyType represents a nullary method type (only if type has kind *)
    // for higher-kinded types, leave PolyType intact
    override def mapOverArgs(args: List[Type], tparams: List[Symbol]): List[Type] =
      map2Conserve(args, tparams) { (arg, tparam) =>
        arg match {
          // is this a higher-kinded position? (TODO: confirm this is the only case)
          case PolyType(tparams, restpe) if tparam.typeParams.nonEmpty =>  // higher-kinded type param
            PolyType(tparams, apply(restpe)) // could not be a nullary method type
          case _ =>
            this(arg)
        }
      }
  }

  private val uncurryType = new TypeMap {
    def apply(tp0: Type): Type = {
      val tp = expandAlias(tp0)
      tp match {
        case ClassInfoType(parents, decls, clazz) =>
          val parents1 = parents mapConserve (uncurry)
          if (parents1 eq parents) tp
          else ClassInfoType(parents1, decls, clazz) // @MAT normalize in decls??
        case PolyType(_, _) =>
          mapOver(tp)
        case _ =>
          tp
      }
    }
  }

  /** - return symbol's transformed type,
   *  - if symbol is a def parameter with transformed type T, return () => T
   *
   * @MAT: starting with this phase, the info of every symbol will be normalized
   */
  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isType) uncurryType(tp) else uncurry(tp)

  /** Traverse tree omitting local method definitions.
   *  If a `return' is encountered, set `returnFound' to true.
   *  Used for MSIL only.
   */
  private object lookForReturns extends Traverser {
    var returnFound = false
    override def traverse(tree: Tree): Unit =  tree match {
      case Return(_) => returnFound = true
      case DefDef(_, _, _, _, _, _) => ;
      case _ => super.traverse(tree)
    }
    def found(tree: Tree) = {
      returnFound = false
      traverse(tree)
      returnFound
    }
  }

  class UnCurryTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    private var needTryLift = false
    private var inPattern = false
    private var inConstructorFlag = 0L
    private val byNameArgs = new HashSet[Tree]
    private val noApply = new HashSet[Tree]

    override def transformUnit(unit: CompilationUnit) {
      freeMutableVars.clear
      freeLocalsTraverser(unit.body)
      unit.body = transform(unit.body)
    }

    private var nprinted = 0

    override def transform(tree: Tree): Tree = try { //debug
      postTransform(mainTransform(tree))
    } catch {
      case ex: Throwable =>
        if (nprinted < 10) {
          Console.println("exception when traversing " + tree)
          nprinted += 1
        }
        throw ex
    }

    /* Is tree a reference `x' to a call by name parameter that neeeds to be converted to
     * x.apply()? Note that this is not the case if `x' is used as an argument to another
     * call by name parameter.
     */
    def isByNameRef(tree: Tree): Boolean =
      tree.isTerm && tree.hasSymbol &&
      tree.symbol.tpe.typeSymbol == ByNameParamClass &&
      !byNameArgs.contains(tree)

    /** Uncurry a type of a tree node.
     *  This function is sensitive to whether or not we are in a pattern -- when in a pattern
     *  additional parameter sections of a case class are skipped.
     */
    def uncurryTreeType(tp: Type): Type = tp match {
      case MethodType(formals, MethodType(formals1, restpe)) if (inPattern) =>
        uncurryTreeType(MethodType(formals, restpe))
      case _ =>
        uncurry(tp)
    }

// ------- Handling non-local returns -------------------------------------------------

    /** The type of a non-local return expression with given argument type */
    private def nonLocalReturnExceptionType(argtype: Type) =
      appliedType(NonLocalReturnExceptionClass.typeConstructor, List(argtype))

    /** A hashmap from method symbols to non-local return keys */
    private val nonLocalReturnKeys = new HashMap[Symbol, Symbol]

    /** Return non-local return key for given method */
    private def nonLocalReturnKey(meth: Symbol) = nonLocalReturnKeys.get(meth) match {
      case Some(k) => k
      case None =>
        val k = meth.newValue(meth.pos, unit.fresh.newName(meth.pos, "nonLocalReturnKey"))
          .setFlag(SYNTHETIC).setInfo(ObjectClass.tpe)
        nonLocalReturnKeys(meth) = k
        k
    }

    /** Generate a non-local return throw with given return expression from given method.
     *  I.e. for the method's non-local return key, generate:
     *
     *    throw new NonLocalReturnException(key, expr)
     *  todo: maybe clone a pre-existing exception instead?
     *  (but what to do about excaptions that miss their targets?)
     */
    private def nonLocalReturnThrow(expr: Tree, meth: Symbol) =
      localTyper.typed {
        Throw(
          New(
            TypeTree(nonLocalReturnExceptionType(expr.tpe)),
            List(List(Ident(nonLocalReturnKey(meth)), expr))))
      }

    /** Transform (body, key) to:
     *
     *  {
     *    val key = new Object()
     *    try {
     *      body
     *    } catch {
     *      case ex: NonLocalReturnException[_] =>
     *        if (ex.key().eq(key)) ex.value()
     *        else throw ex
     *    }
     *  }
     */
    private def nonLocalReturnTry(body: Tree, key: Symbol, meth: Symbol) = {
      localTyper.typed {
        val extpe = nonLocalReturnExceptionType(meth.tpe.finalResultType)
        val ex = meth.newValue(body.pos, nme.ex) setInfo extpe
        val pat = Bind(ex,
                       Typed(Ident(nme.WILDCARD),
                             AppliedTypeTree(Ident(NonLocalReturnExceptionClass),
                                             List(Bind(nme.WILDCARD.toTypeName,
                                                       EmptyTree)))))
        val rhs =
          If(
            Apply(
              Select(
                Apply(Select(Ident(ex), "key"), List()),
                Object_eq),
              List(Ident(key))),
            Apply(
              TypeApply(
                Select(
                  Apply(Select(Ident(ex), "value"), List()),
                  Any_asInstanceOf),
                List(TypeTree(meth.tpe.finalResultType))),
              List()),
            Throw(Ident(ex)))
        val keyDef = ValDef(key, New(TypeTree(ObjectClass.tpe), List(List())))
        val tryCatch = Try(body, List(CaseDef(pat, EmptyTree, rhs)), EmptyTree)
        Block(List(keyDef), tryCatch)
      }
    }

// ------ Transforming anonymous functions and by-name-arguments ----------------

    /** Undo eta expansion for parameterless and nullary methods */
    def deEta(fun: Function): Tree = fun match {
      case Function(List(), Apply(expr, List())) if treeInfo.isPureExpr(expr) =>
        if (expr.hasSymbol && expr.symbol.hasFlag(LAZY))
          fun
        else
          expr
      case Function(List(), expr) if isByNameRef(expr) =>
        noApply += expr
        expr
      case _ =>
        fun
    }


    /*  Transform a function node (x_1,...,x_n) => body of type FunctionN[T_1, .., T_N, R] to
     *
     *    class $anon() extends Object() with FunctionN[T_1, .., T_N, R] with ScalaObject {
     *      def apply(x_1: T_1, ..., x_N: T_n): R = body
     *    }
     *    new $anon()
     *
     *  transform a function node (x => body) of type T =>? R where
     *    body = expr match { case P_i if G_i => E_i }_i=1..n
     *  to:
     *
     *    class $anon() extends Object() with T =>? R with ScalaObject {
     *      def apply(x: T): R = (expr: @unchecked) match {
     *        { case P_i if G_i => E_i }_i=1..n
     *      def isDefinedAt(x: T): boolean = (x: @unchecked) match {
     *        case P_1 if G_1 => true
     *        ...
     *        case P_n if G_n => true
     *        case _ => false
     *      }
     *    }
     *    new $anon()
     *
     *  However, if one of the patterns P_i if G_i is a default pattern, generate instead
     *
     *      def isDefinedAt(x: T): boolean = true
     */
    def transformFunction(fun: Function): Tree = {
      val fun1 = deEta(fun)
      def owner = fun.symbol.owner
      def targs = fun.tpe.typeArgs
      def isPartial = fun.tpe.typeSymbol == PartialFunctionClass

      if (fun1 ne fun) fun1
      else {
        val (formals, restpe) = (targs.init, targs.last)
        val anonClass = owner newAnonymousFunctionClass fun.pos setFlag (FINAL | SYNTHETIC | inConstructorFlag)
        def parents =
          if (isFunctionType(fun.tpe)) List(abstractFunctionForFunctionType(fun.tpe))
          else List(ObjectClass.tpe, fun.tpe)

        anonClass setInfo ClassInfoType(parents, new Scope, anonClass)
        val applyMethod = anonClass.newMethod(fun.pos, nme.apply) setFlag FINAL
        applyMethod setInfo MethodType(applyMethod newSyntheticValueParams formals, restpe)
        anonClass.info.decls enter applyMethod

        fun.vparams foreach (_.symbol.owner = applyMethod)
        new ChangeOwnerTraverser(fun.symbol, applyMethod) traverse fun.body

        def mkUnchecked(tree: Tree) = {
          def newUnchecked(expr: Tree) = Annotated(New(gen.scalaDot(UncheckedClass.name), List(Nil)), expr)
          tree match {
            case Match(selector, cases) => atPos(tree.pos) { Match(newUnchecked(selector), cases) }
            case _                      => tree
          }
        }

        def applyMethodDef() = {
          val body = if (isPartial) mkUnchecked(fun.body) else fun.body
          DefDef(Modifiers(FINAL), nme.apply, Nil, List(fun.vparams), TypeTree(restpe), body) setSymbol applyMethod
        }
        def isDefinedAtMethodDef() = {
          val m = anonClass.newMethod(fun.pos, nme.isDefinedAt) setFlag FINAL
          m setInfo MethodType(m newSyntheticValueParams formals, BooleanClass.tpe)
          anonClass.info.decls enter m

          val Match(selector, cases) = fun.body
          val vparam = fun.vparams.head.symbol
          val idparam = m.paramss.head.head
          val substParam = new TreeSymSubstituter(List(vparam), List(idparam))
          def substTree[T <: Tree](t: T): T = substParam(resetLocalAttrs(t))

          def transformCase(cdef: CaseDef): CaseDef =
            substTree(CaseDef(cdef.pat.duplicate, cdef.guard.duplicate, Literal(true)))
          def defaultCase = CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(false))

          DefDef(m, mkUnchecked(
            if (cases exists treeInfo.isDefaultCase) Literal(true)
            else Match(substTree(selector.duplicate), (cases map transformCase) ::: List(defaultCase))
          ))
        }

        val members =
          if (isPartial) List(applyMethodDef, isDefinedAtMethodDef)
          else List(applyMethodDef)

        localTyper.typed {
          atPos(fun.pos) {
            Block(
              List(ClassDef(anonClass, NoMods, List(List()), List(List()), members, fun.pos)),
              Typed(
                New(TypeTree(anonClass.tpe), List(List())),
                TypeTree(fun.tpe)))
          }
        }
      }
    }

    def transformArgs(pos: Position, fun: Symbol, args: List[Tree], formals: List[Type]) = {
      val isJava = fun hasFlag JAVA
      val args1 = formals.lastOption match {
        case Some(lastFormal) if isRepeatedParamType(lastFormal) =>

          def mkArrayValue(ts: List[Tree], elemtp: Type) =
            ArrayValue(TypeTree(elemtp), ts) setType arrayType(elemtp)

          // when calling into scala varargs, make sure it's a sequence.
          def arrayToSequence(tree: Tree, elemtp: Type) = {
            atPhase(phase.next) {
              localTyper.typedPos(pos) {
                val predef = gen.mkAttributedRef(PredefModule)
                val meth =
                  if ((elemtp <:< AnyRefClass.tpe) && !isPhantomClass(elemtp.typeSymbol))
                    Select(predef, "wrapRefArray")
                  else if (isValueClass(elemtp.typeSymbol))
                    Select(predef, "wrap"+elemtp.typeSymbol.name+"Array")
                  else
                    TypeApply(Select(predef, "genericWrapArray"), List(TypeTree(elemtp)))
                val adaptedTree = // need to cast to Array[elemtp], as arrays are not covariant
                  gen.mkCast(tree, arrayType(elemtp))
                Apply(meth, List(adaptedTree))
              }
            }
          }

          // when calling into java varargs, make sure it's an array - see bug #1360
          def sequenceToArray(tree: Tree) = {
            val toArraySym = tree.tpe member nme.toArray
            assert(toArraySym != NoSymbol)
            def getManifest(tp: Type): Tree = {
              val manifestOpt = localTyper.findManifest(tp, false)
              if (!manifestOpt.tree.isEmpty) manifestOpt.tree
              else if (tp.bounds.hi ne tp) getManifest(tp.bounds.hi)
              else localTyper.getManifestTree(tree.pos, tp, false)
            }
            atPhase(phase.next) {
              localTyper.typedPos(pos) {
                Apply(gen.mkAttributedSelect(tree, toArraySym), List(getManifest(tree.tpe.typeArgs.head)))
              }
            }
          }

          val lastElemType = lastFormal.typeArgs.head
          var suffix: Tree =
            if (!args.isEmpty && (treeInfo isWildcardStarArg args.last)) {
              val Typed(tree, _) = args.last;
              if (isJava)
                if (tree.tpe.typeSymbol == ArrayClass) tree
                else sequenceToArray(tree)
              else
                if (tree.tpe.typeSymbol isSubClass TraversableClass) tree
                else arrayToSequence(tree, lastElemType)
            } else {
              val tree = mkArrayValue(args drop (formals.length - 1), lastElemType)
              if (isJava || inPattern) tree
              else arrayToSequence(tree, lastElemType)
            }

          atPhase(phase.next) {
            if (isJava &&
                suffix.tpe.typeSymbol == ArrayClass &&
                isValueClass(suffix.tpe.typeArgs.head.typeSymbol) &&
                fun.tpe.paramTypes.last.typeSymbol == ArrayClass &&
                fun.tpe.paramTypes.last.typeArgs.head.typeSymbol == ObjectClass)
              suffix = localTyper.typedPos(pos) {
                gen.mkRuntimeCall("toObjectArray", List(suffix))
              }
          }
          args.take(formals.length - 1) ::: List(suffix setType formals.last)
        case _ =>
          args
      }
      (formals, args1).zipped map { (formal, arg) =>
        if (formal.typeSymbol != ByNameParamClass) {
          arg
        } else if (isByNameRef(arg)) {
          byNameArgs.addEntry(arg)
          arg setType functionType(List(), arg.tpe)
        } else {
          val fun = localTyper.typed(
            Function(List(), arg) setPos arg.pos).asInstanceOf[Function];
          new ChangeOwnerTraverser(currentOwner, fun.symbol).traverse(arg);
          transformFunction(fun)
        }
      }
    }

// ------ The tree transformers --------------------------------------------------------

    def mainTransform(tree: Tree): Tree = {

      def withNeedLift(needLift: Boolean)(f: => Tree): Tree = {
        val savedNeedTryLift = needTryLift
        needTryLift = needLift
        val t = f
        needTryLift = savedNeedTryLift
        t
      }

      /** A try or synchronized needs to be lifted anyway for MSIL if it contains
       *  return statements. These are disallowed in the CLR. By lifting
       *  such returns will be converted to throws.
       */
      def shouldBeLiftedAnyway(tree: Tree) = false && // buggy, see #1981
        forMSIL && lookForReturns.found(tree)

      /** Transform tree `t' to { def f = t; f } where `f' is a fresh name
       */
      def liftTree(tree: Tree) = {
        if (settings.debug.value)
          log("lifting tree at: " + (tree.pos))
        val sym = currentOwner.newMethod(tree.pos, unit.fresh.newName(tree.pos, "liftedTree"))
        sym.setInfo(MethodType(List(), tree.tpe))
        new ChangeOwnerTraverser(currentOwner, sym).traverse(tree)
        localTyper.typed {
          atPos(tree.pos) {
            Block(List(DefDef(sym, List(List()), tree)),
                  Apply(Ident(sym), Nil))
          }
        }
      }

      def withInConstructorFlag(inConstructorFlag: Long)(f: => Tree): Tree = {
        val savedInConstructorFlag = this.inConstructorFlag
        this.inConstructorFlag = inConstructorFlag
        val t = f
        this.inConstructorFlag = savedInConstructorFlag
        t
      }

      tree match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          withNeedLift(false) {
            if (tree.symbol.isClassConstructor) {
              atOwner(tree.symbol) {
                val rhs1 = (rhs: @unchecked) match {
                  case Block(stats, expr) =>
                    def transformInConstructor(stat: Tree) =
                      withInConstructorFlag(INCONSTRUCTOR) { transform(stat) }
                    val presupers = treeInfo.preSuperFields(stats) map transformInConstructor
                    val rest = stats drop presupers.length
                    val supercalls = rest take 1 map transformInConstructor
                    val others = rest drop 1 map transform
                    treeCopy.Block(rhs, presupers ::: supercalls ::: others, transform(expr))
                }
                treeCopy.DefDef(
                  tree, mods, name, transformTypeDefs(tparams),
                  transformValDefss(vparamss), transform(tpt), rhs1)
              }
            } else {
              super.transform(tree)
            }
          }

        case ValDef(_, _, _, rhs) =>
          val sym = tree.symbol
          // a local variable that is mutable and free somewhere later should be lifted
          // as lambda lifting (coming later) will wrap 'rhs' in an Ref object.
          if (!sym.owner.isSourceMethod || (sym.isVariable && freeMutableVars(sym)))
            withNeedLift(true) { super.transform(tree) }
          else
            super.transform(tree)
/*
        case Apply(Select(Block(List(), Function(vparams, body)), nme.apply), args) =>
          // perform beta-reduction; this helps keep view applications small
          println("beta-reduce1: "+tree)
          withNeedLift(true) {
            mainTransform(new TreeSubstituter(vparams map (_.symbol), args).transform(body))
          }

        case Apply(Select(Function(vparams, body), nme.apply), args) =>
//        if (List.forall2(vparams, args)((vparam, arg) => treeInfo.isAffineIn(body) ||
//                                        treeInfo.isPureExpr(arg))) =>
          // perform beta-reduction; this helps keep view applications small
          println("beta-reduce2: "+tree)
          withNeedLift(true) {
            mainTransform(new TreeSubstituter(vparams map (_.symbol), args).transform(body))
          }
*/
        case UnApply(fn, args) =>
          inPattern = false
          val fn1 = transform(fn)
          inPattern = true
          val args1 = transformTrees(fn.symbol.name match {
            case nme.unapply    => args
            case nme.unapplySeq => transformArgs(tree.pos, fn.symbol, args, analyzer.unapplyTypeListFromReturnTypeSeq(fn.tpe))
            case _              => Predef.error("internal error: UnApply node has wrong symbol")
          })
          treeCopy.UnApply(tree, fn1, args1)

        case Apply(fn, args) =>
          // XXX settings.noassertions.value temporarily retained to avoid
          // breakage until a reasonable interface is settled upon.
          def elideFunctionCall(sym: Symbol) =
            sym != null && sym.elisionLevel.exists(x => x < settings.elideLevel.value || settings.noassertions.value)

          if (elideFunctionCall(fn.symbol)) {
            Literal(()).setPos(tree.pos).setType(UnitClass.tpe)
          } else if (fn.symbol == Object_synchronized && shouldBeLiftedAnyway(args.head)) {
            transform(treeCopy.Apply(tree, fn, List(liftTree(args.head))))
          } else {
            withNeedLift(true) {
              val formals = fn.tpe.paramTypes;
              treeCopy.Apply(tree, transform(fn), transformTrees(transformArgs(tree.pos, fn.symbol, args, formals)))
            }
          }

        case Assign(Select(_, _), _) =>
          withNeedLift(true) { super.transform(tree) }
        case Assign(lhs, _) if lhs.symbol.owner != currentMethod =>
          withNeedLift(true) { super.transform(tree) }

        case Try(block, catches, finalizer) =>
          if (needTryLift || shouldBeLiftedAnyway(tree)) transform(liftTree(tree))
          else super.transform(tree)

        case CaseDef(pat, guard, body) =>
          inPattern = true
          val pat1 = transform(pat)
          inPattern = false
          treeCopy.CaseDef(tree, pat1, transform(guard), transform(body))

        case fun @ Function(_, _) =>
          mainTransform(transformFunction(fun))

        case Template(_, _, _) =>
          withInConstructorFlag(0) { super.transform(tree) }

        case _ =>
          val tree1 = super.transform(tree)
          if (isByNameRef(tree1)) {
            val tree2 = tree1 setType functionType(List(), tree1.tpe)
            return {
              if (noApply contains tree2) tree2
              else localTyper.typed {
                atPos(tree1.pos) { Apply(Select(tree2, nme.apply), List()) }
              }
            }
          }
          tree1
      }
    } setType uncurryTreeType(tree.tpe)

    def postTransform(tree: Tree): Tree = atPhase(phase.next) {
      def applyUnary(): Tree =
        if (tree.symbol.isMethod &&
            (!tree.tpe.isInstanceOf[PolyType] || tree.tpe.typeParams.isEmpty)) {
          if (!tree.tpe.isInstanceOf[MethodType]) tree.tpe = MethodType(List(), tree.tpe);
          atPos(tree.pos)(Apply(tree, List()) setType tree.tpe.resultType)
        } else if (tree.isType) {
          TypeTree(tree.tpe) setPos tree.pos
        } else {
          tree
        }
      tree match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          val rhs1 = nonLocalReturnKeys.get(tree.symbol) match {
            case None => rhs
            case Some(k) => atPos(rhs.pos)(nonLocalReturnTry(rhs, k, tree.symbol))
          }
          treeCopy.DefDef(tree, mods, name, tparams, List(vparamss.flatten), tpt, rhs1)
        case Try(body, catches, finalizer) =>
          // If warnings are enabled, alert about promiscuously catching cases.
          if (settings.YwarnCatches.value)
            for (cd <- catches find treeInfo.catchesThrowable)
              unit.warning(cd.pos, "catch clause swallows everything: not advised.")

          if (catches forall treeInfo.isCatchCase) tree
          else {
            val exname = unit.fresh.newName(tree.pos, "ex$")
            val cases =
              if ((catches exists treeInfo.isDefaultCase) || (catches.last match {  // bq: handle try { } catch { ... case ex:Throwable => ...}
                    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) if (tpt.tpe =:= ThrowableClass.tpe) =>
                      true
                    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) if (tpt.tpe =:= ThrowableClass.tpe) =>
                      true
                    case _ =>
                      false
                  })) catches
              else catches ::: List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Throw(Ident(exname))));
            val catchall =
              atPos(tree.pos) {
                CaseDef(
                  Bind(exname, Ident(nme.WILDCARD)),
                  EmptyTree,
                  Match(Ident(exname), cases))
              }
            if (settings.debug.value) log("rewrote try: " + catches + " ==> " + catchall);
            val catches1 = localTyper.typedCases(
              tree, List(catchall), ThrowableClass.tpe, WildcardType);
            treeCopy.Try(tree, body, catches1, finalizer)
          }
        case Apply(Apply(fn, args), args1) =>
          treeCopy.Apply(tree, fn, args ::: args1)
        case Ident(name) =>
          assert(name != nme.WILDCARD_STAR.toTypeName)
          applyUnary()
        case Select(_, _) | TypeApply(_, _) =>
          applyUnary()
        case Return(expr) if (tree.symbol != currentOwner.enclMethod || currentOwner.hasFlag(LAZY)) =>
          if (settings.debug.value) log("non local return in "+tree.symbol+" from "+currentOwner.enclMethod)
          atPos(tree.pos)(nonLocalReturnThrow(expr, tree.symbol))
        case TypeTree() =>
          tree
        case _ =>
          if (tree.isType) TypeTree(tree.tpe) setPos tree.pos else tree
      }
    }
  }

  import collection.mutable
  /** Set of mutable local variables that are free in some inner method. */
  private val freeMutableVars: mutable.Set[Symbol] = new mutable.HashSet

  private val freeLocalsTraverser = new Traverser {
    var currentMethod: Symbol = NoSymbol
    override def traverse(tree: Tree) = tree match {
      case DefDef(_, _, _, _, _, _) =>
        val lastMethod = currentMethod
        currentMethod = tree.symbol
        super.traverse(tree)
        currentMethod = lastMethod
      case Ident(_) =>
        val sym = tree.symbol
        if (sym.isVariable && sym.owner.isMethod && sym.owner != currentMethod)
          freeMutableVars += sym
      case _ =>
        super.traverse(tree)
    }
  }
}
