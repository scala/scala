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

package scala
package tools.nsc
package transform

import scala.PartialFunction.cond
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.ListOfNil
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.symtab.Flags._

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
 *  - for every method defining repeated parameters annotated with @varargs, generate
 *    a synthetic Java-style vararg method
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
 *          def liftedTry\$1 = try { x_i } catch { .. }
 *          meth(x_1, .., liftedTry\$1(), .. )
 *        }
 *  - remove calls to elidable methods and replace their bodies with NOPs when elide-below
 *    requires it
 */
abstract class UnCurry extends InfoTransform
                          with scala.reflect.internal.transform.UnCurry
                          with TypingTransformers with ast.TreeDSL {
  val global: Global               // need to repeat here because otherwise last mixin defines global as
                                   // SymbolTable. If we had DOT this would not be an issue
  import CODE._
  import global._
  import definitions._

  val phaseName: String = "uncurry"

  def newTransformer(unit: CompilationUnit): AstTransformer = new UnCurryTransformer(unit)
  override def changesBaseClasses = false

// ------ Type transformation --------------------------------------------------------

// uncurry and uncurryType expand type aliases

  class UnCurryTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    private val forceExpandFunction = settings.Ydelambdafy.value == "inline"
    private var needTryLift       = false
    private var inConstructorFlag = 0L
    private val byNameArgs        = mutable.HashSet[Tree]()
    private val noApply           = mutable.HashSet[Tree]()
    private val newMembers        = mutable.Map[Symbol, mutable.Buffer[Tree]]()

    // Expand `Function`s in constructors to class instance creation (scala/bug#6666, scala/bug#8363)
    // We use Java's LambdaMetaFactory (LMF), which requires an interface for the sam's owner
    private def mustExpandFunction(fun: Function) = {
      // (TODO: Can't use isInterface, yet, as it hasn't been updated for the new trait encoding)
      val canUseLambdaMetaFactory = (fun.attachments.get[SAMFunction] match {
        case Some(SAMFunction(userDefinedSamTp, sam, _)) =>
          // LambdaMetaFactory cannot mix in trait members for us, or instantiate classes -- only pure interfaces need apply
          erasure.compilesToPureInterface(erasure.javaErasure(userDefinedSamTp).typeSymbol) &&
          // impl restriction -- we currently use the boxed apply, so not really useful to allow specialized sam types (https://github.com/scala/scala/pull/4971#issuecomment-198119167)
          // specialization and LMF are at odds, since LMF implements the single abstract method,
          // but that's the one that specialization leaves generic, whereas we need to implement the specialized one to avoid boxing
          !specializeTypes.isSpecializedIn(sam, userDefinedSamTp)

        case _ => true // our built-in FunctionN's are suitable for LambdaMetaFactory by construction
      })

      !canUseLambdaMetaFactory
    }

    /** Add a new synthetic member for `currentOwner` */
    private def addNewMember(t: Tree): Unit =
      newMembers.getOrElseUpdate(currentOwner, mutable.Buffer()) += t

    /** Process synthetic members for `owner`. They are removed form the `newMembers` as a side-effect. */
    @inline private def useNewMembers[T](owner: Symbol)(f: List[Tree] => T): T =
      f(newMembers.remove(owner).getOrElse(Nil).toList)

    // I don't have a clue why I'm catching TypeErrors here, but it's better
    // than spewing stack traces at end users for internal errors. Examples
    // which hit at this point should not be hard to come by, but the immediate
    // motivation can be seen in continuations-neg/t3718.
    override def transform(tree: Tree): Tree =
      try postTransform(mainTransform(tree))
      catch { case ex: TypeError =>
        reporter.error(ex.pos, ex.msg)
        debugStack(ex)
        EmptyTree
      }

    /* Is tree a reference `x` to a call by name parameter that needs to be converted to
     * x.apply()? Note that this is not the case if `x` is used as an argument to another
     * call by name parameter.
     */
    def isByNameRef(tree: Tree) = (
         tree.isTerm
      && (tree.symbol ne null)
      && !(tree.symbol.hasPackageFlag || tree.isInstanceOf[This] || tree.isInstanceOf[Super])
      && isByName(tree.symbol)
      && !byNameArgs(tree)
    )

// ------- Handling non-local returns -------------------------------------------------

    /** The type of a non-local return expression with given argument type */
    private def nonLocalReturnExceptionType(argtype: Type) =
      appliedType(NonLocalReturnControlClass, argtype :: Nil)

    /** A hashmap from method symbols to non-local return keys */
    private val nonLocalReturnKeys = perRunCaches.newMap[Symbol, Symbol]()

    /** Return non-local return key for given method */
    private def nonLocalReturnKey(meth: Symbol) =
      nonLocalReturnKeys.getOrElseUpdate(meth,
        meth.newValue(unit.freshTermName(nme.NON_LOCAL_RETURN_KEY_STRING), meth.pos, SYNTHETIC) setInfo ObjectTpe
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
        val restpe  = meth.tpe_*.finalResultType
        val extpe   = nonLocalReturnExceptionType(restpe)
        val ex      = meth.newValue(nme.ex, body.pos) setInfo extpe
        val argType = restpe withAnnotation (AnnotationInfo marker UncheckedClass.tpe)
        val pat     = gen.mkBindForCase(ex, NonLocalReturnControlClass, List(argType))
        val rhs     = (
          IF   ((ex DOT nme.key)() OBJ_EQ Ident(key))
          THEN ((ex DOT nme.value)())
          ELSE (Throw(Ident(ex)))
        )
        val keyDef   = ValDef(key, New(ObjectTpe))
        val tryCatch = Try(body, pat -> rhs)

        import treeInfo.{catchesThrowable, isSyntheticCase}
        for {
          Try(_, catches, _) <- body
          cdef <- catches
          if catchesThrowable(cdef) && !isSyntheticCase(cdef)
        } {
          runReporting.warning(body.pos, "catch block may intercept non-local return from " + meth, WarningCategory.Other, meth)
        }

        Block(List(keyDef), tryCatch)
      }
    }

// ------ Transforming anonymous functions and by-name-arguments ----------------

    /**  Transform a function node (x_1,...,x_n) => body of type FunctionN[T_1, .., T_N, R] to
     *
     *    class \$anon() extends AbstractFunctionN[T_1, .., T_N, R] with Serializable {
     *      def apply(x_1: T_1, ..., x_N: T_n): R = body
     *    }
     *    new \$anon()
     *
     */
    def transformFunction(fun: Function): Tree =
      // Undo eta expansion for parameterless and nullary methods, EXCEPT if `fun` targets a SAM.
      // Normally, we can unwrap `() => cbn` to `cbn` where `cbn` refers to a CBN argument (typically `cbn` is an Ident),
      // because we know `cbn` will already be a `Function0` thunk. When we're targeting a SAM,
      // the types don't align and we must preserve the function wrapper.
      if (fun.vparams.isEmpty && isByNameRef(fun.body) && fun.attachments.get[SAMFunction].isEmpty) { noApply += fun.body ; fun.body }
      else if (forceExpandFunction || inConstructorFlag != 0) {
        // Expand the function body into an anonymous class
        gen.expandFunction(localTyper)(fun, inConstructorFlag)
      } else {
        val mustExpand = mustExpandFunction(fun)
        // method definition with the same arguments, return type, and body as the original lambda
        val liftedMethod = gen.mkLiftedFunctionBodyMethod(localTyper)(fun.symbol.owner, fun)

        // new function whose body is just a call to the lifted method
        val newFun = deriveFunction(fun)(_ => localTyper.typedPos(fun.pos)(
          gen.mkForwarder(gen.mkAttributedRef(liftedMethod.symbol), (fun.vparams map (_.symbol)) :: Nil)
        ))

        if (!mustExpand) {
          liftedMethod.symbol.updateAttachment(DelambdafyTarget)
          liftedMethod.updateAttachment(DelambdafyTarget)
        }

        val typedNewFun = localTyper.typedPos(fun.pos)(Block(liftedMethod :: Nil, super.transform(newFun)))
        if (mustExpand) {
          val Block(stats, expr : Function) = typedNewFun: @unchecked
          treeCopy.Block(typedNewFun, stats, gen.expandFunction(localTyper)(expr, inConstructorFlag))
        } else {
          typedNewFun
        }
      }

    def transformArgs(pos: Position, fun: Symbol, args: List[Tree], params: List[Symbol]): List[Tree] = {
      val isJava = fun.isJavaDefined

      def transformVarargs(varargsElemType: Type): List[Tree] = {
        def mkArrayValue(ts: List[Tree], elemtp: Type) =
          ArrayValue(TypeTree(elemtp), ts) setType arrayType(elemtp)

        // when calling into scala varargs, make sure it's a sequence.
        def arrayToSequence(tree: Tree, elemtp: Type, copy: Boolean): Tree = {
          exitingUncurry {
            localTyper.typedPos(pos) {
              val pt = arrayType(elemtp)
              val adaptedTree = // might need to cast to Array[elemtp], as arrays are not covariant
                if (tree.tpe <:< pt) tree
                else gen.mkCastArray(tree, elemtp, pt)

              if(copy) {
                runReporting.deprecationWarning(tree.pos, NoSymbol, currentOwner,
                  "Passing an explicit array value to a Scala varargs method is deprecated (since 2.13.0) and will result in a defensive copy; "+
                    "Use the more efficient non-copying ArraySeq.unsafeWrapArray or an explicit toIndexedSeq call", "2.13.0")
                gen.mkMethodCall(PredefModule, nme.copyArrayToImmutableIndexedSeq, List(elemtp), List(adaptedTree))
              } else gen.mkWrapVarargsArray(adaptedTree, elemtp)
            }
          }
        }

        // when calling into java varargs, make sure it's an array - see bug #1360
        def sequenceToArray(tree: Tree): Tree = {
          val toArraySym = tree.tpe member nme.toArray
          assert(toArraySym != NoSymbol, "toArray")
          @tailrec
          def getClassTag(tp: Type): Tree = {
            val tag = localTyper.resolveClassTag(tree.pos, tp)
            // Don't want bottom types getting any further than this (scala/bug#4024)
            if (tp.typeSymbol.isBottomClass) getClassTag(AnyTpe)
            else if (!tag.isEmpty) tag
            else if (tp.upperBound ne tp) getClassTag(tp.upperBound)
            else localTyper.TyperErrorGen.MissingClassTagError(tree, tp)
          }
          def iterableClassTag(tpe: Type): Tree = {
            (tpe baseType IterableClass).typeArgs match {
              case targ :: _  => getClassTag(targ)
              case _          => EmptyTree
            }
          }
          exitingUncurry {
            localTyper.typedPos(pos) {
              gen.mkMethodCall(tree, toArraySym, Nil, List(iterableClassTag(tree.tpe)))
            }
          }
        }

        /* Java-style varargs = expects `Array` rather than `Seq`
         * Note that `fun.isJavaDefined` is not good enough because
         * if we override a varargs method defined in Java, `superaccessors`
         * will make us a superaccessor which also takes `Array` rather than `Seq`.
         * See scala/bug#10368 */
        val javaStyleVarArgs = isJavaVarArgsMethod(fun)
        var suffix: Tree =
          if (treeInfo isWildcardStarArgList args) {
            val Typed(tree, _) = args.last: @unchecked
            if (javaStyleVarArgs)
              if (tree.tpe.typeSymbol == ArrayClass) tree
              else sequenceToArray(tree)
            else
              if (tree.tpe.typeSymbol isSubClass SeqClass) tree
              else arrayToSequence(tree, varargsElemType, copy = true) // existing array, make a defensive copy
          }
          else {
            def mkArray = mkArrayValue(args drop (params.length - 1), varargsElemType)
            // if args.length < params.length the repeated argument is empty
            def emptyVarargs = compareLengths(args, params) < 0
            if (javaStyleVarArgs) mkArray
            else if (emptyVarargs) gen.mkNil  // avoid needlessly double-wrapping an empty argument list
            else arrayToSequence(mkArray, varargsElemType, copy = false) // fresh array, no need to copy
          }

        exitingUncurry {
          if (isJava && !isReferenceArray(suffix.tpe) && isArrayOfSymbol(fun.tpe.params.last.tpe, ObjectClass)) {
            // The array isn't statically known to be a reference array, so call ScalaRuntime.toObjectArray.
            suffix = localTyper.typedPos(pos) {
              gen.mkRuntimeCall(nme.toObjectArray, List(suffix))
            }
          }
        }
        val args1 = ListBuffer[Tree]()
        args1 ++= args.iterator.take(params.length - 1)
        args1 += suffix setType params.last.info
        args1.toList
      }

      val isVarargs = isVarArgsList(params)
      val args1 = if (isVarargs) transformVarargs(params.last.info.typeArgs.head.widen) else args

      map2Conserve(args1, params) { (arg, param) =>
        if (!isByNameParamType(param.info)) arg
        else if (isByNameRef(arg)) { // thunk does not need to be forced because it's a reference to a by-name arg passed to a by-name param
          byNameArgs += arg
          arg setType functionType(Nil, arg.tpe)
        } else {
          log(s"Argument '$arg' at line ${arg.pos.line} is ${param.info} from ${fun.fullName}")
          def canUseDirectly(qual: Tree) = qual.tpe.typeSymbol.isSubClass(FunctionClass(0)) && treeInfo.isExprSafeToInline(qual)
          arg match {
            // don't add a thunk for by-name argument if argument already is an application of
            // a Function0. We can then remove the application and use the existing Function0.
            case Apply(Select(qual, nme.apply), Nil) if canUseDirectly(qual) => qual
            case body =>
              val thunkFun = localTyper.typedPos(body.pos)(Function(Nil, body)).asInstanceOf[Function]
              log(s"Change owner from $currentOwner to ${thunkFun.symbol} in ${thunkFun.body}")
              thunkFun.body.changeOwner(currentOwner, thunkFun.symbol)
              transformFunction(thunkFun)
          }
        }
      }
    }

    /** Called if a tree's symbol is elidable.  If it's a DefDef,
     *  replace only the body/rhs with 0/false/()/null; otherwise replace
     *  the whole tree with it.
     */
    private def replaceElidableTree(tree: Tree): Tree = {
      def elisionOf(t: Type): Tree = t.typeSymbol match {
        case StringClass => Literal(Constant("")) setType t
        case _ => gen.mkZero(t)
      }
      tree match {
        case DefDef(_,_,_,_,_,rhs) =>
          val rhs1 = if (rhs == EmptyTree) rhs else Block(Nil, elisionOf(rhs.tpe)) setType rhs.tpe
          deriveDefDef(tree)(_ => rhs1) setSymbol tree.symbol setType tree.tpe
        case _ =>
          elisionOf(tree.tpe)
      }
    }

    private def isSelfSynchronized(ddef: DefDef) = ddef.rhs match {
      case Apply(fn @ TypeApply(Select(sel, _), _), _) =>
        fn.symbol == Object_synchronized && sel.symbol == ddef.symbol.enclClass && !ddef.symbol.enclClass.isTrait &&
          !ddef.symbol.isDelambdafyTarget /* these become static later, unsuitable for ACC_SYNCHRONIZED */
      case _ => false
    }

    /** If an eligible method is entirely wrapped in a call to synchronized
     *  locked on the same instance, remove the synchronized scaffolding and
     *  mark the method symbol SYNCHRONIZED for bytecode generation.
     *
     *  Delambdafy targets are deemed ineligible as the Delambdafy phase will
     *  replace `this.synchronized` with `\$this.synchronized` now that it emits
     *  all lambda impl methods as static.
     */
    private def translateSynchronized(tree: Tree) = tree match {
      case dd @ DefDef(_, _, _, _, _, Apply(_, body :: Nil)) if isSelfSynchronized(dd) =>
        log("Translating " + dd.symbol.defString + " into synchronized method")
        dd.symbol setFlag SYNCHRONIZED
        deriveDefDef(dd)(_ => body)
      case _ => tree
    }
    def isNonLocalReturn(ret: Return) = ret.symbol != currentOwner.enclMethod || currentOwner.isLazy || currentOwner.isAnonymousFunction

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
        val sym = currentOwner.newMethod(unit.freshTermName(nme.LIFTED_TREE), tree.pos, Flag.ARTIFACT)
        sym.setInfo(MethodType(List(), tree.tpe))
        tree.changeOwner(currentOwner, sym)
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

      // true if the target is a lambda body that's been lifted into a method
      def isLiftedLambdaMethod(funSym: Symbol) =
        funSym.isArtifact && funSym.name.containsName(nme.ANON_FUN_NAME) && funSym.isLocalToBlock

      def checkIsElidable(sym: Symbol): Boolean = (sym ne null) && sym.elisionLevel.exists { level =>
          if (sym.isMethod) level < settings.elidebelow.value
          else {
            // TODO: report error? It's already done in RefChecks. https://github.com/scala/scala/pull/5539#issuecomment-331376887
            reporter.error(sym.pos, s"${sym.name}: Only methods can be marked @elidable.")
            false
          }
        }

      val result =
        if (checkIsElidable(sym))
          replaceElidableTree(tree)
        else translateSynchronized(tree) match {
          case dd @ DefDef(mods, name, tparams, _, tpt, rhs) =>
            // Remove default argument trees from parameter ValDefs, scala/bug#4812
            val vparamssNoRhs = dd.vparamss mapConserve (_ mapConserve {p =>
              treeCopy.ValDef(p, p.mods, p.name, p.tpt, EmptyTree)
            })

            if (dd.symbol hasAnnotation VarargsClass) validateVarargs(dd)

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
          case ValDef(mods, _, _, rhs) =>
            if (sym eq NoSymbol) throw new IllegalStateException("Encountered Valdef without symbol: "+ tree + " in "+ unit)
            if (!sym.owner.isSourceMethod || mods.isLazy)
              withNeedLift(needLift = true) { super.transform(tree) }
            else
              super.transform(tree)

          case sel: Select if sel.qualifier.tpe.typeSymbol.isDerivedValueClass =>
            // `c.f` where `c` is a value class is translated to `C.f$extension(c)` (value class member) or
            // `new C(c).f()` (universal trait member). In both cases, `try` within `c` needs a lift.
            withNeedLift(needLift = true) { super.transform(tree) }

          case Apply(fn, args) =>
            // Read the param symbols before `transform(fn)`, because UnCurry replaces T* by Seq[T] (see DesugaredParameterType).
            // The call to `transformArgs` below needs `formals` that still have varargs.
            val fnParams = fn.tpe.params
            val transformedFn = transform(fn)
            // scala/bug#6479: no need to lift in args to label jumps
            // scala/bug#11127: boolean && / || are emitted using jumps, the lhs stack value is consumed by the conditional jump
            val noReceiverOnStack = fn.symbol.isLabel || fn.symbol == currentRun.runDefinitions.Boolean_and || fn.symbol == currentRun.runDefinitions.Boolean_or
            val needLift = needTryLift || !noReceiverOnStack
            withNeedLift(needLift) {
              treeCopy.Apply(tree, transformedFn, transformTrees(transformArgs(tree.pos, fn.symbol, args, fnParams)))
            }

          case Assign(_: RefTree, _) =>
            withNeedLift(needLift = true) { super.transform(tree) }

          case Assign(lhs, _) if lhs.symbol.owner != currentMethod || lhs.symbol.hasFlag(LAZY | ACCESSOR) =>
            withNeedLift(needLift = true) { super.transform(tree) }

          case ret @ Return(_) if isNonLocalReturn(ret) =>
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
            val pat1 = transform(pat)
            treeCopy.CaseDef(tree, pat1, transform(guard), transform(body))

          // if a lambda is already the right shape we don't need to transform it again
          case fun @ Function(_, Apply(target, _)) if !forceExpandFunction && isLiftedLambdaMethod(target.symbol) =>
            super.transform(fun)

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

      result.setType(uncurry(result.tpe))
    }

    def postTransform(tree: Tree): Tree = exitingUncurry {
      def applyUnary(): Tree = {
        // TODO_NMT: verify that the inner tree of a type-apply also gets parens if the
        // whole tree is a polymorphic nullary method application
        def removeNullary() = tree.tpe match {
          case MethodType(_, _)           => tree
          case tp                         => tree setType MethodType(Nil, tp.resultType)
        }
        val sym = tree.symbol
        // our info transformer may not have run yet, so duplicate flag logic instead of forcing it to run
        val isMethodExitingUncurry = (sym hasFlag METHOD) || (sym hasFlag MODULE) && !sym.isStatic
        if (isMethodExitingUncurry && !tree.tpe.isInstanceOf[PolyType])
          gen.mkApplyIfNeeded(removeNullary()) // apply () if tree.tpe has zero-arg MethodType
        else if (tree.isType)
          TypeTree(tree.tpe) setPos tree.pos
        else
          tree
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
          val ddSym = dd.symbol
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

          // A no-arg method with ConstantType result type can safely be reduced to the corresponding Literal
          // (only pure methods are typed as ConstantType). We could also do this for methods with arguments,
          // after ensuring the arguments are not referenced.
          val literalRhsIfConst =
            if (newParamss.head.isEmpty) { // We know newParamss.length == 1 from above
              ddSym.info.resultType match {
                case tp@FoldableConstantType(value) => Literal(value) setType tp setPos newRhs.pos // inlining of gen.mkAttributedQualifier(tp)
                case _ => newRhs
              }
            } else newRhs

          val flatdd = copyDefDef(dd)(
            vparamss = newParamss,
            rhs = nonLocalReturnKeys get ddSym match {
              case Some(k) => atPos(newRhs.pos)(nonLocalReturnTry(literalRhsIfConst, k, ddSym))
              case None    => literalRhsIfConst
            }
          )
          // Only class members can reasonably be called from Java due to name mangling.
          // Additionally, the Uncurry info transformer only adds a forwarder symbol to class members,
          // since the other symbols are not part of the ClassInfoType (see reflect.internal.transform.UnCurry)
          if (dd.symbol.owner.isClass)
            addJavaVarargsForwarders(dd, flatdd)
          else
            flatdd

        case tree: Try =>
          devWarningIf(tree.catches exists (!treeInfo.isCatchCase(_))) {
            "VPM BUG - illegal try/catch " + tree.catches
          }
          tree

        case Apply(Apply(fn, args), args1) =>
          treeCopy.Apply(tree, fn, args ::: args1)

        case Ident(name) =>
          assert(name != tpnme.WILDCARD_STAR, tree)
          applyUnary()
        case Select(_, _) | TypeApply(_, _) =>
          applyUnary()
        case ret @ Return(expr) if isNonLocalReturn(ret) =>
          log(s"non-local return from ${currentOwner.enclMethod} to ${ret.symbol}")
          if (settings.warnNonlocalReturn)
            runReporting.warning(ret.pos, s"return statement uses an exception to pass control to the caller of the enclosing named ${ret.symbol}", WarningCategory.LintNonlocalReturn, ret.symbol)
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
     * such a signature overrides a corresponding signature in a parent. (scala/bug#6443).
     *
     * This transformation erases the dependent method types by:
     *   - Widening the formal parameter type to existentially abstract
     *     over the prior parameters (using `packSymbols`). This transformation
     *     is performed in the `InfoTransform`er [[scala.reflect.internal.transform.UnCurry]].
     *   - Inserting casts in the method body to cast to the original,
     *     precise type.
     *
     * For the example above, this results in:
     *
     * {{{
     *   def foo(a: A, b: a.B forSome { val a: A }): a.B = { val b\$1 = b.asInstanceOf[a.B]; b\$1; b\$1 }
     * }}}
     */
    private object dependentParamTypeErasure {

      def isDependent(dd: DefDef): Boolean =
        enteringUncurry {
          val methType = dd.symbol.info
          methType.isDependentMethodType && mexists(methType.paramss)(_.info exists (_.isImmediatelyDependent))
        }

      /**
       * @return (newVparamss, newRhs)
       */
      def erase(dd: DefDef): (List[List[ValDef]], Tree) = {
        import dd.{rhs, vparamss}
        val (allParams, packedParamsSyms, tempVals): (List[ValDef], List[Symbol], List[ValDef]) = {

          val allParamsBuf: ListBuffer[ValDef] = ListBuffer.empty
          val packedParamsSymsBuf: ListBuffer[Symbol] = ListBuffer.empty
          val tempValsBuf: ListBuffer[ValDef] = ListBuffer.empty

          def addPacked(param: ValDef, tempVal: ValDef): Unit = {
            allParamsBuf += param
            if (rhs != EmptyTree) {
              packedParamsSymsBuf += param.symbol
              tempValsBuf         += tempVal
            }
          }

          def addParamTransform(p: ValDef, infoParam: Symbol): Unit = {
            val packedType = infoParam.info
            if (packedType =:= p.symbol.info) allParamsBuf += p
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
                // scala/bug#9442: using the "uncurry-erased" type (the one after the uncurry phase) can lead to incorrect
                // tree transformations. For example, compiling:
                //
                //   def foo(c: Ctx)(l: c.Tree): Unit = {
                //     val l2: c.Tree = l
                //   }
                //
                // Results in the following AST:
                //
                //   def foo(c: Ctx, l: Ctx#Tree): Unit = {
                //     val l$1: Ctx#Tree = l.asInstanceOf[Ctx#Tree]
                //     val l2: c.Tree = l$1 // no, not really, it's not.
                //   }
                //
                // Of course, this is incorrect, since `l$1` has type `Ctx#Tree`, which is not a subtype of `c.Tree`.
                //
                // So what we need to do is to use the pre-uncurry type when creating `l$1`, which is `c.Tree` and is
                // correct. Now, there are two additional problems:
                // 1. when varargs and byname params are involved, the uncurry transformation desugars these special
                //    cases to actual typerefs, eg:
                //
                //           T*   ~> Seq[T] (Scala-defined varargs)
                //           T*   ~> Array[T] (Java-defined varargs)
                //           => T ~> Function0[T] (by name params)
                //
                //    we use the DesugaredParameterType object (defined in scala.reflect.internal.transform.UnCurry)
                //    to redo this desugaring manually here
                // 2. the type needs to be normalized, since `gen.mkCast` checks this (no HK here, just aliases have
                //    to be expanded before handing the type to `gen.mkAttributedCast`, which calls `gen.mkCast`)
                val info0 =
                  enteringUncurry(p.symbol.info) match {
                    case DesugaredParameterType(desugaredTpe) =>
                      desugaredTpe
                    case tpe =>
                      tpe
                  }
                val info = info0.normalize
                val tempValName = unit.freshTermName(p.name.toStringWithSuffix("$"))
                val newSym = dd.symbol.newTermSymbol(tempValName, p.pos, SYNTHETIC).setInfo(info)
                atPos(p.pos)(ValDef(newSym, gen.mkAttributedCast(Ident(p.symbol), info)))
              }
              addPacked(newParam, tempVal)
            }
          }

          val viter = vparamss.iterator.flatten
          val piter = dd.symbol.info.paramss.iterator.flatten
          while (viter.hasNext && piter.hasNext)
            addParamTransform(viter.next(), piter.next())

          (allParamsBuf.toList, packedParamsSymsBuf.toList, tempValsBuf.toList)
        }

        val rhs1 = if (rhs == EmptyTree || tempVals.isEmpty) rhs else {
          localTyper.typedPos(rhs.pos) {
            // Patch the method body to refer to the temp vals
            val rhsSubstituted = rhs.substituteSymbols(packedParamsSyms, tempVals.map(_.symbol))
            // The new method body: { val p$1 = p.asInstanceOf[<dependent type>]; ...; <rhsSubstituted> }
            Block(tempVals, rhsSubstituted)
          }
        }

        (allParams :: Nil, rhs1)
      }
    }

    private def validateVarargs(dd: DefDef): Unit =
      if (dd.symbol.isConstructor)
        reporter.error(dd.symbol.pos, "A constructor cannot be annotated with a `varargs` annotation.")
      else {
        val ok = cond(dd.symbol.paramss.filter(_.nonEmpty)) {
          case initPs :+ lastPs =>
            initPs.forall(!definitions.isVarArgsList(_)) && definitions.isVarArgsList(lastPs)
        }
        if (!ok)
          reporter.error(dd.pos, "A method annotated with @varargs must have a single repeated parameter in its last parameter list.")
      }

    /**
     * Called during post transform, after the method argument lists have been flattened.
     * It looks for the forwarder symbol in the symbol attachments and generates a Java-style
     * varargs forwarder.
     *
     * @note The Java-style varargs method symbol is generated in the Uncurry info transformer. If the
     *       symbol can't be found this method reports a warning and carries on.
     * @see  [[scala.reflect.internal.transform.UnCurry]]
     */
    private def addJavaVarargsForwarders(dd: DefDef, flatdd: DefDef): DefDef = {
      if (!dd.symbol.hasAnnotation(VarargsClass) || !enteringUncurry(mexists(dd.symbol.paramss)(sym => definitions.isRepeatedParamType(sym.tpe))))
        return flatdd

      val forwSym: Symbol = {
        currentClass.info // make sure the info is up to date, so the varargs forwarder symbol has been generated
        flatdd.symbol.attachments.get[VarargsSymbolAttachment] match {
          case Some(VarargsSymbolAttachment(sym)) => sym
          case None =>
            runReporting.warning(dd.pos, s"Could not generate Java varargs forwarder for ${flatdd.symbol}. Please file a bug.", WarningCategory.Other, dd.symbol)
            return flatdd
        }
      }

      val newPs = forwSym.tpe.params
      val isRepeated = enteringUncurry(dd.symbol.info.paramss.flatten.map(sym => definitions.isRepeatedParamType(sym.tpe)))
      val oldPs = flatdd.symbol.paramss.head

      val theTyper = typer.atOwner(dd, currentClass)
      val forwTree = theTyper.typedPos(dd.pos) {
        val seqArgs = map3(newPs, oldPs, isRepeated)((param, _, isRep) => {
          if (!isRep) Ident(param)
          else {
            val parTp = elementType(ArrayClass, param.tpe)
            val wrap = gen.mkWrapVarargsArray(Ident(param), parTp)
            param.attachments.get[TypeParamVarargsAttachment] match {
              case Some(TypeParamVarargsAttachment(tp)) => gen.mkCast(wrap, seqType(tp))
              case _ => wrap
            }
          }
        })

        val forwCall = Apply(gen.mkAttributedRef(flatdd.symbol), seqArgs)
        DefDef(forwSym, if (forwSym.isConstructor) Block(List(forwCall), UNIT) else forwCall)
      }

      // check if the method with that name and those arguments already exists in the template
      enteringUncurry(currentClass.info.member(forwSym.name).alternatives.find(s => s != forwSym && s.tpe.matches(forwSym.tpe))) match {
        case Some(s) =>
          reporter.error(dd.symbol.pos,
            s"A method annotated with @varargs produces a forwarder method with the same signature ${s.tpe} as an existing method.")
        case None =>
          // enter symbol into scope
          addNewMember(forwTree)
      }

      flatdd
    }
  }
}
