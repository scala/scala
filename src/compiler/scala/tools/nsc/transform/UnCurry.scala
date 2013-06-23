/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author
 */

package scala
package tools.nsc
package transform

import symtab.Flags._
import scala.collection.{ mutable, immutable }
import scala.language.postfixOps

/*<export> */
/** - uncurry all symbol and tree types (@see UnCurryPhase) -- this includes normalizing all proper types.
 *  - for every curried parameter list:  (ps_1) ... (ps_n) ==> (ps_1, ..., ps_n)
 *  - for every curried application: f(args_1)...(args_n) ==> f(args_1, ..., args_n)
 *  - for every type application: f[Ts] ==> f[Ts]() unless followed by parameters
 *  - for every use of a parameterless function: f ==> f()  and  q.f ==> q.f()
 *  - for every def-parameter:  x: => T ==> x: () => T
 *  - for every use of a def-parameter: x ==> x.apply()
 *  - for every argument to a def parameter `x: => T':
 *      if argument is not a reference to a def parameter:
 *        convert argument `e` to (expansion of) `() => e'
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
 *  - convert try-catch expressions in contexts where there might be values on the stack to
 *      a local method and a call to it (since an exception empties the evaluation stack):
 *
 *      meth(x_1,..., try { x_i } catch { ..}, .. x_b0) ==>
 *        {
 *          def liftedTry$1 = try { x_i } catch { .. }
 *          meth(x_1, .., liftedTry$1(), .. )
 *        }
 */
/*</export> */
abstract class UnCurry extends InfoTransform
                          with scala.reflect.internal.transform.UnCurry
                          with TypingTransformers with ast.TreeDSL {
  val global: Global               // need to repeat here because otherwise last mixin defines global as
                                   // SymbolTable. If we had DOT this would not be an issue
  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import CODE._

  val phaseName: String = "uncurry"

  def newTransformer(unit: CompilationUnit): Transformer = new UnCurryTransformer(unit)
  override def changesBaseClasses = false

// ------ Type transformation --------------------------------------------------------

// uncurry and uncurryType expand type aliases

  class UnCurryTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    private var needTryLift       = false
    private var inPattern         = false
    private var inConstructorFlag = 0L
    private val byNameArgs        = mutable.HashSet[Tree]()
    private val noApply           = mutable.HashSet[Tree]()
    private val newMembers        = mutable.Map[Symbol, mutable.Buffer[Tree]]()
    private val repeatedParams    = mutable.Map[Symbol, List[ValDef]]()

    /** Add a new synthetic member for `currentOwner` */
    private def addNewMember(t: Tree): Unit =
      newMembers.getOrElseUpdate(currentOwner, mutable.Buffer()) += t

    /** Process synthetic members for `owner`. They are removed form the `newMembers` as a side-effect. */
    @inline private def useNewMembers[T](owner: Symbol)(f: List[Tree] => T): T =
      f(newMembers.remove(owner).getOrElse(Nil).toList)

    @inline private def withInPattern[T](value: Boolean)(body: => T): T = {
      inPattern = value
      try body
      finally inPattern = !value
    }

    private def newFunction0(body: Tree): Tree = {
      val result = localTyper.typedPos(body.pos)(Function(Nil, body)).asInstanceOf[Function]
      log("Change owner from %s to %s in %s".format(currentOwner, result.symbol, result.body))
      result.body changeOwner (currentOwner -> result.symbol)
      transformFunction(result)
    }

    private lazy val serialVersionUIDAnnotation =
      AnnotationInfo(SerialVersionUIDAttr.tpe, List(Literal(Constant(0))), List())

    // I don't have a clue why I'm catching TypeErrors here, but it's better
    // than spewing stack traces at end users for internal errors. Examples
    // which hit at this point should not be hard to come by, but the immediate
    // motivation can be seen in continuations-neg/t3718.
    override def transform(tree: Tree): Tree = (
      try postTransform(mainTransform(tree))
      catch { case ex: TypeError =>
        unit.error(ex.pos, ex.msg)
        debugStack(ex)
        EmptyTree
      }
    )

    /* Is tree a reference `x` to a call by name parameter that needs to be converted to
     * x.apply()? Note that this is not the case if `x` is used as an argument to another
     * call by name parameter.
     */
    def isByNameRef(tree: Tree) = (
         tree.isTerm
      && !byNameArgs(tree)
      && (tree.symbol ne null)
      && (isByName(tree.symbol))
    )

    /** Uncurry a type of a tree node.
     *  This function is sensitive to whether or not we are in a pattern -- when in a pattern
     *  additional parameter sections of a case class are skipped.
     */
    def uncurryTreeType(tp: Type): Type = tp match {
      case MethodType(params, MethodType(params1, restpe)) if inPattern =>
        uncurryTreeType(MethodType(params, restpe))
      case _ =>
        uncurry(tp)
    }

// ------- Handling non-local returns -------------------------------------------------

    /** The type of a non-local return expression with given argument type */
    private def nonLocalReturnExceptionType(argtype: Type) =
      appliedType(NonLocalReturnControlClass, argtype)

    /** A hashmap from method symbols to non-local return keys */
    private val nonLocalReturnKeys = perRunCaches.newMap[Symbol, Symbol]()

    /** Return non-local return key for given method */
    private def nonLocalReturnKey(meth: Symbol) =
      nonLocalReturnKeys.getOrElseUpdate(meth,
        meth.newValue(unit.freshTermName("nonLocalReturnKey"), meth.pos, SYNTHETIC) setInfo ObjectTpe
      )

    /** Generate a non-local return throw with given return expression from given method.
     *  I.e. for the method's non-local return key, generate:
     *
     *    throw new NonLocalReturnControl(key, expr)
     *  todo: maybe clone a pre-existing exception instead?
     *  (but what to do about exceptions that miss their targets?)
     */
    private def nonLocalReturnThrow(expr: Tree, meth: Symbol) = localTyper typed {
      Throw(
        nonLocalReturnExceptionType(expr.tpe.widen),
        Ident(nonLocalReturnKey(meth)),
        expr
      )
    }

    /** Transform (body, key) to:
     *
     *  {
     *    val key = new Object()
     *    try {
     *      body
     *    } catch {
     *      case ex: NonLocalReturnControl[T @unchecked] =>
     *        if (ex.key().eq(key)) ex.value()
     *        else throw ex
     *    }
     *  }
     */
    private def nonLocalReturnTry(body: Tree, key: Symbol, meth: Symbol) = {
      localTyper typed {
        val extpe   = nonLocalReturnExceptionType(meth.tpe.finalResultType)
        val ex      = meth.newValue(nme.ex, body.pos) setInfo extpe
        val argType = meth.tpe.finalResultType withAnnotation (AnnotationInfo marker UncheckedClass.tpe)
        val pat     = gen.mkBindForCase(ex, NonLocalReturnControlClass, List(argType))
        val rhs = (
          IF   ((ex DOT nme.key)() OBJ_EQ Ident(key))
          THEN ((ex DOT nme.value)())
          ELSE (Throw(Ident(ex)))
        )
        val keyDef   = ValDef(key, New(ObjectTpe))
        val tryCatch = Try(body, pat -> rhs)

        import treeInfo.{catchesThrowable, isSyntheticCase}
        for {
          Try(t, catches, _) <- body
          cdef <- catches
          if catchesThrowable(cdef) && !isSyntheticCase(cdef)
        } {
          unit.warning(body.pos, "catch block may intercept non-local return from " + meth)
        }

        Block(List(keyDef), tryCatch)
      }
    }

// ------ Transforming anonymous functions and by-name-arguments ----------------

    /** Undo eta expansion for parameterless and nullary methods */
    def deEta(fun: Function): Tree = fun match {
      case Function(List(), expr) if isByNameRef(expr) =>
        noApply += expr
        expr
      case _ =>
        fun
    }


    /**  Transform a function node (x_1,...,x_n) => body of type FunctionN[T_1, .., T_N, R] to
     *
     *    class $anon() extends AbstractFunctionN[T_1, .., T_N, R] with Serializable {
     *      def apply(x_1: T_1, ..., x_N: T_n): R = body
     *    }
     *    new $anon()
     *
     */
    def transformFunction(fun: Function): Tree = {
      fun.tpe match {
        // can happen when analyzer plugins assign refined types to functions, e.g.
        // (() => Int) { def apply(): Int @typeConstraint }
        case RefinedType(List(funTp), decls) =>
          debuglog(s"eliminate refinement from function type ${fun.tpe}")
          fun.tpe = funTp
        case _ =>
          ()
      }

      deEta(fun) match {
        // nullary or parameterless
        case fun1 if fun1 ne fun => fun1
        case _ =>
          val parents = addSerializable(abstractFunctionForFunctionType(fun.tpe))
          val anonClass = fun.symbol.owner newAnonymousFunctionClass(fun.pos, inConstructorFlag) addAnnotation serialVersionUIDAnnotation
          anonClass setInfo ClassInfoType(parents, newScope, anonClass)

          val targs     = fun.tpe.typeArgs
          val (formals, restpe) = (targs.init, targs.last)

          val applyMethodDef = {
            val methSym = anonClass.newMethod(nme.apply, fun.pos, FINAL)
            val paramSyms = map2(formals, fun.vparams) {
              (tp, param) => methSym.newSyntheticValueParam(tp, param.name)
            }
            methSym setInfoAndEnter MethodType(paramSyms, restpe)

            fun.vparams foreach  (_.symbol.owner =  methSym)
            fun.body changeOwner (fun.symbol     -> methSym)

            val body    = localTyper.typedPos(fun.pos)(fun.body)
            val methDef = DefDef(methSym, List(fun.vparams), body)

            // Have to repack the type to avoid mismatches when existentials
            // appear in the result - see SI-4869.
            methDef.tpt setType localTyper.packedType(body, methSym)
            methDef
          }

          localTyper.typedPos(fun.pos) {
            Block(
              List(ClassDef(anonClass, NoMods, ListOfNil, List(applyMethodDef), fun.pos)),
              Typed(New(anonClass.tpe), TypeTree(fun.tpe)))
          }

      }
    }

    def transformArgs(pos: Position, fun: Symbol, args: List[Tree], formals: List[Type]) = {
      val isJava = fun.isJavaDefined
      def transformVarargs(varargsElemType: Type) = {
        def mkArrayValue(ts: List[Tree], elemtp: Type) =
          ArrayValue(TypeTree(elemtp), ts) setType arrayType(elemtp)

        // when calling into scala varargs, make sure it's a sequence.
        def arrayToSequence(tree: Tree, elemtp: Type) = {
          exitingUncurry {
            localTyper.typedPos(pos) {
              val pt = arrayType(elemtp)
              val adaptedTree = // might need to cast to Array[elemtp], as arrays are not covariant
                if (tree.tpe <:< pt) tree
                else gen.mkCastArray(tree, elemtp, pt)

              gen.mkWrapArray(adaptedTree, elemtp)
            }
          }
        }

        // when calling into java varargs, make sure it's an array - see bug #1360
        def sequenceToArray(tree: Tree) = {
          val toArraySym = tree.tpe member nme.toArray
          assert(toArraySym != NoSymbol)
          def getClassTag(tp: Type): Tree = {
            val tag = localTyper.resolveClassTag(tree.pos, tp)
            // Don't want bottom types getting any further than this (SI-4024)
            if (tp.typeSymbol.isBottomClass) getClassTag(AnyTpe)
            else if (!tag.isEmpty) tag
            else if (tp.bounds.hi ne tp) getClassTag(tp.bounds.hi)
            else localTyper.TyperErrorGen.MissingClassTagError(tree, tp)
          }
          def traversableClassTag(tpe: Type): Tree = {
            (tpe baseType TraversableClass).typeArgs match {
              case targ :: _  => getClassTag(targ)
              case _          => EmptyTree
            }
          }
          exitingUncurry {
            localTyper.typedPos(pos) {
              gen.mkMethodCall(tree, toArraySym, Nil, List(traversableClassTag(tree.tpe)))
            }
          }
        }

        var suffix: Tree =
          if (treeInfo isWildcardStarArgList args) {
            val Typed(tree, _) = args.last
            if (isJava)
              if (tree.tpe.typeSymbol == ArrayClass) tree
              else sequenceToArray(tree)
            else
              if (tree.tpe.typeSymbol isSubClass SeqClass) tree
              else arrayToSequence(tree, varargsElemType)
          }
          else {
            def mkArray = mkArrayValue(args drop (formals.length - 1), varargsElemType)
            if (isJava || inPattern) mkArray
            else if (args.isEmpty) gen.mkNil  // avoid needlessly double-wrapping an empty argument list
            else arrayToSequence(mkArray, varargsElemType)
          }

        exitingUncurry {
          if (isJava && !isReferenceArray(suffix.tpe) && isArrayOfSymbol(fun.tpe.params.last.tpe, ObjectClass)) {
            // The array isn't statically known to be a reference array, so call ScalaRuntime.toObjectArray.
            suffix = localTyper.typedPos(pos) {
              gen.mkRuntimeCall(nme.toObjectArray, List(suffix))
            }
          }
        }
        args.take(formals.length - 1) :+ (suffix setType formals.last)
      }

      val args1 = if (isVarArgTypes(formals)) transformVarargs(formals.last.typeArgs.head) else args

      map2(formals, args1) { (formal, arg) =>
        if (!isByNameParamType(formal))
          arg
        else if (isByNameRef(arg)) {
          byNameArgs += arg
          arg setType functionType(Nil, arg.tpe)
        }
        else {
          log(s"Argument '$arg' at line ${arg.pos.safeLine} is $formal from ${fun.fullName}")
          def canUseDirectly(recv: Tree) = (
               recv.tpe.typeSymbol.isSubClass(FunctionClass(0))
            && treeInfo.isExprSafeToInline(recv)
          )
          arg match {
            // don't add a thunk for by-name argument if argument already is an application of
            // a Function0. We can then remove the application and use the existing Function0.
            case Apply(Select(recv, nme.apply), Nil) if canUseDirectly(recv) =>
              recv
            case _ =>
              newFunction0(arg)
          }
        }
      }
    }

    /** Called if a tree's symbol is elidable.  If it's a DefDef,
     *  replace only the body/rhs with 0/false/()/null; otherwise replace
     *  the whole tree with it.
     */
    private def replaceElidableTree(tree: Tree): Tree = {
      tree match {
        case DefDef(_,_,_,_,_,_) =>
          deriveDefDef(tree)(rhs => Block(Nil, gen.mkZero(rhs.tpe)) setType rhs.tpe) setSymbol tree.symbol setType tree.tpe
        case _ =>
          gen.mkZero(tree.tpe) setType tree.tpe
      }
    }

    private def isSelfSynchronized(ddef: DefDef) = ddef.rhs match {
      case Apply(fn @ TypeApply(Select(sel, _), _), _) =>
        fn.symbol == Object_synchronized && sel.symbol == ddef.symbol.enclClass && !ddef.symbol.enclClass.isTrait
      case _ => false
    }

    /** If an eligible method is entirely wrapped in a call to synchronized
     *  locked on the same instance, remove the synchronized scaffolding and
     *  mark the method symbol SYNCHRONIZED for bytecode generation.
     */
    private def translateSynchronized(tree: Tree) = tree match {
      case dd @ DefDef(_, _, _, _, _, Apply(fn, body :: Nil)) if isSelfSynchronized(dd) =>
        log("Translating " + dd.symbol.defString + " into synchronized method")
        dd.symbol setFlag SYNCHRONIZED
        deriveDefDef(dd)(_ => body)
      case _ => tree
    }
    def isNonLocalReturn(ret: Return) = ret.symbol != currentOwner.enclMethod || currentOwner.isLazy

// ------ The tree transformers --------------------------------------------------------

    def mainTransform(tree: Tree): Tree = {
      @inline def withNeedLift(needLift: Boolean)(f: => Tree): Tree = {
        val saved = needTryLift
        needTryLift = needLift
        try f
        finally needTryLift = saved
      }

      /* Transform tree `t` to { def f = t; f } where `f` is a fresh name */
      def liftTree(tree: Tree) = {
        debuglog("lifting tree at: " + (tree.pos))
        val sym = currentOwner.newMethod(unit.freshTermName("liftedTree"), tree.pos)
        sym.setInfo(MethodType(List(), tree.tpe))
        tree.changeOwner(currentOwner -> sym)
        localTyper.typedPos(tree.pos)(Block(
          List(DefDef(sym, ListOfNil, tree)),
          Apply(Ident(sym), Nil)
        ))
      }

      def withInConstructorFlag(inConstructorFlag: Long)(f: => Tree): Tree = {
        val saved = this.inConstructorFlag
        this.inConstructorFlag = inConstructorFlag
        try f
        finally this.inConstructorFlag = saved
      }

      val sym = tree.symbol
      val result = (
        // TODO - settings.noassertions.value temporarily retained to avoid
        // breakage until a reasonable interface is settled upon.
        if ((sym ne null) && (sym.elisionLevel.exists (_ < settings.elidebelow.value || settings.noassertions)))
          replaceElidableTree(tree)
        else translateSynchronized(tree) match {
          case dd @ DefDef(mods, name, tparams, _, tpt, rhs) =>
            // Remove default argument trees from parameter ValDefs, SI-4812
            val vparamssNoRhs = dd.vparamss mapConserve (_ mapConserve {p =>
              treeCopy.ValDef(p, p.mods, p.name, p.tpt, EmptyTree)
            })

            if (dd.symbol hasAnnotation VarargsClass) saveRepeatedParams(dd)

            withNeedLift(needLift = false) {
              if (dd.symbol.isClassConstructor) {
                atOwner(sym) {
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
                    dd, mods, name, transformTypeDefs(tparams),
                    transformValDefss(vparamssNoRhs), transform(tpt), rhs1)
                }
              } else {
                super.transform(treeCopy.DefDef(dd, mods, name, tparams, vparamssNoRhs, tpt, rhs))
              }
            }
          case ValDef(_, _, _, rhs) =>
            if (sym eq NoSymbol) throw new IllegalStateException("Encountered Valdef without symbol: "+ tree + " in "+ unit)
            if (!sym.owner.isSourceMethod)
              withNeedLift(needLift = true) { super.transform(tree) }
            else
              super.transform(tree)
          case UnApply(fn, args) =>
            val fn1 = withInPattern(value = false)(transform(fn))
            val args1 = transformTrees(fn.symbol.name match {
              case nme.unapply    => args
              case nme.unapplySeq => transformArgs(tree.pos, fn.symbol, args, analyzer.unapplyTypeList(fn.pos, fn.symbol, fn.tpe, args))
              case _              => sys.error("internal error: UnApply node has wrong symbol")
            })
            treeCopy.UnApply(tree, fn1, args1)

          case Apply(fn, args) =>
            val needLift = needTryLift || !fn.symbol.isLabel // SI-6749, no need to lift in args to label jumps.
            withNeedLift(needLift) {
              val formals = fn.tpe.paramTypes
              treeCopy.Apply(tree, transform(fn), transformTrees(transformArgs(tree.pos, fn.symbol, args, formals)))
            }

          case Assign(_: RefTree, _) =>
            withNeedLift(needLift = true) { super.transform(tree) }

          case Assign(lhs, _) if lhs.symbol.owner != currentMethod || lhs.symbol.hasFlag(LAZY | ACCESSOR) =>
            withNeedLift(needLift = true) { super.transform(tree) }

          case ret @ Return(_) if (isNonLocalReturn(ret)) =>
            withNeedLift(needLift = true) { super.transform(ret) }

          case Try(_, Nil, _) =>
            // try-finally does not need lifting: lifting is needed only for try-catch
            // expressions that are evaluated in a context where the stack might not be empty.
            // `finally` does not attempt to continue evaluation after an exception, so the fact
            // that values on the stack are 'lost' does not matter
            super.transform(tree)

          case Try(block, catches, finalizer) =>
            if (needTryLift) transform(liftTree(tree))
            else super.transform(tree)

          case CaseDef(pat, guard, body) =>
            val pat1 = withInPattern(value = true)(transform(pat))
            treeCopy.CaseDef(tree, pat1, transform(guard), transform(body))

          case fun @ Function(_, _) =>
            mainTransform(transformFunction(fun))

          case Template(_, _, _) =>
            withInConstructorFlag(0) { super.transform(tree) }

          case _ =>
            val tree1 = super.transform(tree)
            if (isByNameRef(tree1)) {
              val tree2 = tree1 setType functionType(Nil, tree1.tpe)
              return {
                if (noApply contains tree2) tree2
                else localTyper.typedPos(tree1.pos)(Apply(Select(tree2, nme.apply), Nil))
              }
            }
            tree1
        }
      )
      assert(result.tpe != null, result.shortClass + " tpe is null:\n" + result)
      result setType uncurryTreeType(result.tpe)
    }

    def postTransform(tree: Tree): Tree = exitingUncurry {
      def applyUnary(): Tree = {
        // TODO_NMT: verify that the inner tree of a type-apply also gets parens if the
        // whole tree is a polymorphic nullary method application
        def removeNullary() = tree.tpe match {
          case MethodType(_, _)           => tree
          case tp                         => tree setType MethodType(Nil, tp.resultType)
        }
        if (tree.symbol.isMethod && !tree.tpe.isInstanceOf[PolyType])
          gen.mkApplyIfNeeded(removeNullary())
        else if (tree.isType)
          TypeTree(tree.tpe) setPos tree.pos
        else
          tree
      }

      def isThrowable(pat: Tree): Boolean = pat match {
        case Typed(Ident(nme.WILDCARD), tpt) =>
          tpt.tpe =:= ThrowableTpe
        case Bind(_, pat) =>
          isThrowable(pat)
        case _ =>
          false
      }

      tree match {
        /* Some uncurry post transformations add members to templates.
         *
         * Members registered by `addMembers` for the current template are added
         * once the template transformation has finished.
         *
         * In particular, this case will add:
         * - synthetic Java varargs forwarders for repeated parameters
         */
        case Template(_, _, _) =>
          localTyper = typer.atOwner(tree, currentClass)
          useNewMembers(currentClass) {
            newMembers =>
              deriveTemplate(tree)(transformTrees(newMembers) ::: _)
          }

        case dd @ DefDef(_, _, _, vparamss0, _, rhs0) =>
          val (newParamss, newRhs): (List[List[ValDef]], Tree) =
            if (dependentParamTypeErasure isDependent dd)
              dependentParamTypeErasure erase dd
            else {
              val vparamss1 = vparamss0 match {
                case _ :: Nil => vparamss0
                case _        => vparamss0.flatten :: Nil
              }
              (vparamss1, rhs0)
            }

          val flatdd = copyDefDef(dd)(
            vparamss = newParamss,
            rhs = nonLocalReturnKeys get dd.symbol match {
              case Some(k) => atPos(newRhs.pos)(nonLocalReturnTry(newRhs, k, dd.symbol))
              case None    => newRhs
            }
          )
          addJavaVarargsForwarders(dd, flatdd)

        case tree: Try =>
          if (tree.catches exists (cd => !treeInfo.isCatchCase(cd)))
            devWarning("VPM BUG - illegal try/catch " + tree.catches)
          tree

        case Apply(Apply(fn, args), args1) =>
          treeCopy.Apply(tree, fn, args ::: args1)

        case Ident(name) =>
          assert(name != tpnme.WILDCARD_STAR, tree)
          applyUnary()
        case Select(_, _) | TypeApply(_, _) =>
          applyUnary()
        case ret @ Return(expr) if isNonLocalReturn(ret) =>
          log("non-local return from %s to %s".format(currentOwner.enclMethod, ret.symbol))
          atPos(ret.pos)(nonLocalReturnThrow(expr, ret.symbol))
        case TypeTree() =>
          tree
        case _ =>
          if (tree.isType) TypeTree(tree.tpe) setPos tree.pos else tree
      }
    }

    /**
     * When we concatenate parameter lists, formal parameter types that were dependent
     * on prior parameter values will no longer be correctly scoped.
     *
     * For example:
     *
     * {{{
     *   def foo(a: A)(b: a.B): a.type = {b; b}
     *   // after uncurry
     *   def foo(a: A, b: a/* NOT IN SCOPE! */.B): a.B = {b; b}
     * }}}
     *
     * This violates the principle that each compiler phase should produce trees that
     * can be retyped (see [[scala.tools.nsc.typechecker.TreeCheckers]]), and causes
     * a practical problem in `erasure`: it is not able to correctly determine if
     * such a signature overrides a corresponding signature in a parent. (SI-6443).
     *
     * This transformation erases the dependent method types by:
     *   - Widening the formal parameter type to existentially abstract
     *     over the prior parameters (using `packSymbols`). This transformation
     *     is performed in the the `InfoTransform`er [[scala.reflect.internal.transform.UnCurry]].
     *   - Inserting casts in the method body to cast to the original,
     *     precise type.
     *
     * For the example above, this results in:
     *
     * {{{
     *   def foo(a: A, b: a.B forSome { val a: A }): a.B = { val b$1 = b.asInstanceOf[a.B]; b$1; b$1 }
     * }}}
     */
    private object dependentParamTypeErasure {
      sealed abstract class ParamTransform {
        def param: ValDef
      }
      final case class Identity(param: ValDef) extends ParamTransform
      final case class Packed(param: ValDef, tempVal: ValDef) extends ParamTransform

      def isDependent(dd: DefDef): Boolean =
        enteringUncurry {
          val methType = dd.symbol.info
          methType.isDependentMethodType && mexists(methType.paramss)(_.info exists (_.isImmediatelyDependent))
        }

      /**
       * @return (newVparamss, newRhs)
       */
      def erase(dd: DefDef): (List[List[ValDef]], Tree) = {
        import dd.{ vparamss, rhs }
        val paramTransforms: List[ParamTransform] =
          map2(vparamss.flatten, dd.symbol.info.paramss.flatten) { (p, infoParam) =>
            val packedType = infoParam.info
            if (packedType =:= p.symbol.info) Identity(p)
            else {
              // The Uncurry info transformer existentially abstracted over value parameters
              // from the previous parameter lists.

              // Change the type of the param symbol
              p.symbol updateInfo packedType

              // Create a new param tree
              val newParam: ValDef = copyValDef(p)(tpt = TypeTree(packedType))

              // Within the method body, we'll cast the parameter to the originally
              // declared type and assign this to a synthetic val. Later, we'll patch
              // the method body to refer to this, rather than the parameter.
              val tempVal: ValDef = {
                val tempValName = unit freshTermName (p.name + "$")
                val newSym = dd.symbol.newTermSymbol(tempValName, p.pos, SYNTHETIC).setInfo(p.symbol.info)
                atPos(p.pos)(ValDef(newSym, gen.mkAttributedCast(Ident(p.symbol), p.symbol.info)))
              }
              Packed(newParam, tempVal)
            }
          }

        val allParams = paramTransforms map (_.param)
        val (packedParams, tempVals) = paramTransforms.collect {
          case Packed(param, tempVal) => (param, tempVal)
        }.unzip

        val rhs1 = localTyper.typedPos(rhs.pos) {
          // Patch the method body to refer to the temp vals
          val rhsSubstituted = rhs.substituteSymbols(packedParams map (_.symbol), tempVals map (_.symbol))
          // The new method body: { val p$1 = p.asInstanceOf[<dependent type>]; ...; <rhsSubstituted> }
          Block(tempVals, rhsSubstituted)
        }

        (allParams :: Nil, rhs1)
      }
    }


    /* Analyzes repeated params if method is annotated as `varargs`.
     * If the repeated params exist, it saves them into the `repeatedParams` map,
     * which is used later.
     */
    private def saveRepeatedParams(dd: DefDef): Unit =
      if (dd.symbol.isConstructor)
        unit.error(dd.symbol.pos, "A constructor cannot be annotated with a `varargs` annotation.")
      else treeInfo.repeatedParams(dd) match {
        case Nil  =>
          unit.error(dd.symbol.pos, "A method without repeated parameters cannot be annotated with the `varargs` annotation.")
        case reps =>
          repeatedParams(dd.symbol) = reps
      }

    /* Called during post transform, after the method argument lists have been flattened.
     * It looks for the method in the `repeatedParams` map, and generates a Java-style
     * varargs forwarder.
     */
    private def addJavaVarargsForwarders(dd: DefDef, flatdd: DefDef): DefDef = {
      if (!dd.symbol.hasAnnotation(VarargsClass) || !repeatedParams.contains(dd.symbol))
        return flatdd

      def toArrayType(tp: Type): Type = {
        val arg = elementType(SeqClass, tp)
        // to prevent generation of an `Object` parameter from `Array[T]` parameter later
        // as this would crash the Java compiler which expects an `Object[]` array for varargs
        //   e.g.        def foo[T](a: Int, b: T*)
        //   becomes     def foo[T](a: Int, b: Array[Object])
        //   instead of  def foo[T](a: Int, b: Array[T]) ===> def foo[T](a: Int, b: Object)
        arrayType(
          if (arg.typeSymbol.isTypeParameterOrSkolem) ObjectTpe
          else arg
        )
      }

      val reps       = repeatedParams(dd.symbol)
      val rpsymbols  = reps.map(_.symbol).toSet
      val theTyper   = typer.atOwner(dd, currentClass)
      val flatparams = flatdd.vparamss.head

      // create the type
      val forwformals = flatparams map {
        case p if rpsymbols(p.symbol) => toArrayType(p.symbol.tpe)
        case p                        => p.symbol.tpe
      }
      val forwresult = dd.symbol.tpe.finalResultType
      val forwformsyms = map2(forwformals, flatparams)((tp, oldparam) =>
        currentClass.newValueParameter(oldparam.name, oldparam.symbol.pos).setInfo(tp)
      )
      def mono = MethodType(forwformsyms, forwresult)
      val forwtype = dd.symbol.tpe match {
        case MethodType(_, _) => mono
        case PolyType(tps, _) => PolyType(tps, mono)
      }

      // create the symbol
      val forwsym = currentClass.newMethod(dd.name.toTermName, dd.pos, VARARGS | SYNTHETIC | flatdd.symbol.flags) setInfo forwtype

      // create the tree
      val forwtree = theTyper.typedPos(dd.pos) {
        val locals = map2(forwsym ARGS, flatparams) {
          case (_, fp) if !rpsymbols(fp.symbol) => null
          case (argsym, fp)                     =>
            Block(Nil,
              gen.mkCast(
                gen.mkWrapArray(Ident(argsym), elementType(ArrayClass, argsym.tpe)),
                seqType(elementType(SeqClass, fp.symbol.tpe))
              )
            )
        }
        val seqargs = map2(locals, forwsym ARGS) {
          case (null, argsym) => Ident(argsym)
          case (l, _)         => l
        }
        val end = if (forwsym.isConstructor) List(UNIT) else Nil

        DEF(forwsym) === BLOCK(
          Apply(gen.mkAttributedRef(flatdd.symbol), seqargs) :: end : _*
        )
      }

      // check if the method with that name and those arguments already exists in the template
      currentClass.info.member(forwsym.name).alternatives.find(s => s != forwsym && s.tpe.matches(forwsym.tpe)) match {
        case Some(s) => unit.error(dd.symbol.pos,
                                   "A method with a varargs annotation produces a forwarder method with the same signature "
                                   + s.tpe + " as an existing method.")
        case None =>
          // enter symbol into scope
          currentClass.info.decls enter forwsym
          addNewMember(forwtree)
      }

      flatdd
    }
  }
}
