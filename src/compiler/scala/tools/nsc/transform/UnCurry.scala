/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author
 */
// $Id$

package scala.tools.nsc.transform

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
 *  - for every repeated parameter `x: T*' --> x: Seq[T].
 *  - for every argument list that corresponds to a repeated parameter
 *       (a_1, ..., a_n) => (Seq(a_1, ..., a_n))
 *  - for every argument list that is an escaped sequence
 *       (a_1:_*) => (a_1)g
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

//@MAT: uncurry and uncurryType fully expand type aliases in their input and output
// note: don't normalize higher-kined types -- @M TODO: maybe split those uses of normalize?
// OTOH, should be a problem as calls to normalize only occur on types with kind * in principle (in well-typed programs)
  private def expandAlias(tp: Type): Type = if (!tp.isHigherKinded) tp.normalize else tp

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
        case PolyType(List(), restpe) =>
          apply(MethodType(List(), restpe))
        case PolyType(tparams, restpe) =>
          PolyType(tparams, apply(MethodType(List(), restpe)))
        case TypeRef(pre, sym, List(arg)) if (sym == ByNameParamClass) =>
          apply(functionType(List(), arg))
        case TypeRef(pre, sym, args) if (sym == RepeatedParamClass) =>
          apply(appliedType(SeqClass.typeConstructor, args))
        case _ =>
          expandAlias(mapOver(tp))
      }
    }
  }

  private val uncurryType = new TypeMap {
    def apply(tp0: Type): Type = {
      val tp = expandAlias(tp0)
      tp match {
        case ClassInfoType(parents, decls, clazz) =>
          val parents1 = List.mapConserve(parents)(uncurry)
          if (parents1 eq parents) tp
          else ClassInfoType(parents1, decls, clazz) // @MAT normalize in decls??
        case PolyType(_, _) =>
          mapOver(tp)
        case _ =>
          tp
      }
    }
  }

  /** Convert repeated parameters to arrays if they occur as part of a Java method
   */
  private def repeatedToArray(tp: Type): Type = tp match {
    case MethodType(params, rtpe)
    if (!params.isEmpty && params.last.tpe.typeSymbol == RepeatedParamClass) =>
      val arrayParam = params.last.owner.newSyntheticValueParams(List(
        appliedType(ArrayClass.typeConstructor, List(params.last.tpe.typeArgs.head))))
      MethodType(params.init ::: arrayParam, rtpe)
    case PolyType(tparams, rtpe) =>
      val rtpe1 = repeatedToArray(rtpe)
      if (rtpe1 eq rtpe) tp
      else PolyType(tparams, rtpe1)
    case _ =>
      tp
  }

  /** - return symbol's transformed type,
   *  - if symbol is a def parameter with transformed type T, return () => T
   *
   * @MAT: starting with this phase, the info of every symbol will be normalized
   */
  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isType) uncurryType(tp)
    else if (sym.isMethod && sym.hasFlag(JAVA)) uncurry(repeatedToArray(tp))
    else uncurry(tp)

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
    override def transform(tree: Tree): Tree = try { //debug
      postTransform(mainTransform(tree))
    } catch {
      case ex: Throwable =>
        Console.println("exception when traversing " + tree)
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
     *  transform a function node (x => body) of type PartialFunction[T, R] where
     *    body = x match { case P_i if G_i => E_i }_i=1..n
     *  to:
     *
     *    class $anon() extends Object() with PartialFunction[T, R] with ScalaObject {
     *      def apply(x: T): R = (x: @unchecked) match {
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
      if (fun1 ne fun) fun1
      else {
        val anonClass = fun.symbol.owner.newAnonymousFunctionClass(fun.pos)
          .setFlag(FINAL | SYNTHETIC | inConstructorFlag)
        val formals = fun.tpe.typeArgs.init
        val restpe = fun.tpe.typeArgs.last
        anonClass setInfo ClassInfoType(
          List(ObjectClass.tpe, fun.tpe, ScalaObjectClass.tpe), newScope, anonClass);
        val applyMethod = anonClass.newMethod(fun.pos, nme.apply).setFlag(FINAL)
        applyMethod.setInfo(MethodType(applyMethod.newSyntheticValueParams(formals), restpe))
        anonClass.info.decls enter applyMethod;
        for (vparam <- fun.vparams) vparam.symbol.owner = applyMethod;
        new ChangeOwnerTraverser(fun.symbol, applyMethod).traverse(fun.body);
        def applyMethodDef(body: Tree) =
          DefDef(Modifiers(FINAL), nme.apply, List(), List(fun.vparams), TypeTree(restpe), body)
            .setSymbol(applyMethod)
/*
        def toStringMethodDefs = fun match {
          case etaExpansion(_, fn, _) if (fn.hasSymbol) =>
            List(
              DefDef(Modifiers(FINAL | OVERRIDE), nme.toString_, List(), List(List()), TypeTree(StringClass.tpe),
                     Literal(fn.symbol.name)))
          case _ =>
            List()
        }
*/
        def mkUnchecked(tree: Tree) = tree match {
          case Match(selector, cases) =>
            atPos(tree.pos) {
              Match(
                Annotated(Annotation(New(TypeTree(UncheckedClass.tpe), List(List())), List()), selector),
                cases)
            }
          case _ =>
            tree
        }
        val members = {
          if (fun.tpe.typeSymbol == PartialFunctionClass) {
            val isDefinedAtMethod = anonClass.newMethod(fun.pos, nme.isDefinedAt).setFlag(FINAL)
            isDefinedAtMethod.setInfo(MethodType(isDefinedAtMethod.newSyntheticValueParams(formals),
                                                 BooleanClass.tpe))
            anonClass.info.decls enter isDefinedAtMethod
            def idbody(idparam: Symbol) = fun.body match {
              case Match(_, cases) =>
                val substParam = new TreeSymSubstituter(List(fun.vparams.head.symbol), List(idparam));
                def transformCase(cdef: CaseDef): CaseDef =
                  substParam(
                    resetAttrs(
                      CaseDef(cdef.pat.duplicate, cdef.guard.duplicate, Literal(true))))
                if (cases exists treeInfo.isDefaultCase) Literal(true)
                else
                  Match(
                    Ident(idparam),
                    (cases map transformCase) :::
                      List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(false))))
            }
            List(applyMethodDef(mkUnchecked(fun.body)),
                 DefDef(isDefinedAtMethod, mkUnchecked(idbody(isDefinedAtMethod.paramss.head.head))))
          } else {
            List(applyMethodDef(fun.body))
          }
        } /* ::: toStringMethodDefs */
        localTyper.typed {
          atPos(fun.pos) {
            Block(
              List(ClassDef(anonClass, NoMods, List(List()), List(List()), members)),
              Typed(
                New(TypeTree(anonClass.tpe), List(List())),
                TypeTree(fun.tpe)))

          }
        }
      }
    }

    def transformArgs(pos: Position, args: List[Tree], formals: List[Type], isJava: Boolean) = {
      if (formals.isEmpty) {
        assert(args.isEmpty); List()
      } else {
        val args1 =
          formals.last match {
            case TypeRef(pre, sym, List(elempt)) if (sym == RepeatedParamClass) =>
              def mkArrayValue(ts: List[Tree]) =
                atPos(pos)(ArrayValue(TypeTree(elempt), ts) setType formals.last);

              // when calling into java varargs, make sure it's an array - see bug #1360
              def forceToArray(arg: Tree) = {
                val Typed(tree, _) = arg
                lazy val isTraversable = tree.tpe.baseClasses contains TraversableClass
                lazy val toArray = tree.tpe member nme.toArray

                if (isJava && isTraversable && toArray != NoSymbol)
                  Apply(gen.mkAttributedSelect(tree, toArray), Nil) setType tree.tpe.memberType(toArray)
                else
                  tree
              }

              if (args.isEmpty)
                List(mkArrayValue(args))
              else {
                val suffix: Tree =
                  if (treeInfo isWildcardStarArg args.last) forceToArray(args.last)
                  else mkArrayValue(args drop (formals.length - 1))

                args.take(formals.length - 1) ::: List(suffix)
              }
            case _ => args
          }
        List.map2(formals, args1) { (formal, arg) =>
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
          val args1 = transformTrees(
            if (fn.symbol.name == nme.unapply)
              args
            else if (fn.symbol.name == nme.unapplySeq)
              transformArgs(tree.pos, args, analyzer.unapplyTypeListFromReturnTypeSeq(fn.tpe), false)
            else { assert(false,"internal error: UnApply node has wrong symbol"); null })
          treeCopy.UnApply(tree, fn1, args1)

        case Apply(fn, args) =>
          if (settings.noassertions.value &&
              (fn.symbol ne null) &&
              (fn.symbol.name == nme.assert_ || fn.symbol.name == nme.assume_) &&
              fn.symbol.owner == PredefModule.moduleClass) {
            Literal(()).setPos(tree.pos).setType(UnitClass.tpe)
          } else if (fn.symbol == Object_synchronized && shouldBeLiftedAnyway(args.head)) {
            transform(treeCopy.Apply(tree, fn, List(liftTree(args.head))))
          } else {
            withNeedLift(true) {
              val formals = fn.tpe.paramTypes;
              val isJava = fn.symbol hasFlag JAVA // in case we need a varargs transformation
              treeCopy.Apply(tree, transform(fn), transformTrees(transformArgs(tree.pos, args, formals, isJava)))
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
      def applyUnary(tree: Tree): Tree =
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
          treeCopy.DefDef(tree, mods, name, tparams, List(List.flatten(vparamss)), tpt, rhs1)
        case Try(body, catches, finalizer) =>
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
          applyUnary(tree);
        case Select(qual, name) =>
          applyUnary(tree)
        case TypeApply(_, _) =>
          applyUnary(tree)
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
