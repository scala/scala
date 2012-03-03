/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author
 */

package scala.tools.nsc
package transform

import symtab.Flags._
import scala.collection.{ mutable, immutable }

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
/*</export> */
abstract class UnCurry extends InfoTransform with TypingTransformers with ast.TreeDSL {
  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import CODE._

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
      // tp0.typeSymbolDirect.initialize
      val tp = expandAlias(tp0)
      tp match {
        case MethodType(params, MethodType(params1, restpe)) =>
          apply(MethodType(params ::: params1, restpe))
        case MethodType(params, ExistentialType(tparams, restpe @ MethodType(_, _))) =>
          assert(false, "unexpected curried method types with intervening existential")
          tp0
        case MethodType(h :: t, restpe) if h.isImplicit =>
          apply(MethodType(h.cloneSymbol.resetFlag(IMPLICIT) :: t, restpe))
        case NullaryMethodType(restpe) =>
          apply(MethodType(List(), restpe))
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
  }

  private val uncurryType = new TypeMap {
    def apply(tp0: Type): Type = {
      val tp = expandAlias(tp0)
      tp match {
        case ClassInfoType(parents, decls, clazz) =>
          val parents1 = parents mapConserve uncurry
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
    private val byNameArgs = new mutable.HashSet[Tree]
    private val noApply = new mutable.HashSet[Tree]
    private val newMembers = mutable.ArrayBuffer[Tree]()
    private val repeatedParams = mutable.Map[Symbol, List[ValDef]]()

    private lazy val serialVersionUIDAnnotation =
      AnnotationInfo(SerialVersionUIDAttr.tpe, List(Literal(Constant(0))), List())

    override def transformUnit(unit: CompilationUnit) {
      freeMutableVars.clear()
      freeLocalsTraverser(unit.body)
      super.transformUnit(unit)
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

    /* Is tree a reference `x' to a call by name parameter that needs to be converted to
     * x.apply()? Note that this is not the case if `x' is used as an argument to another
     * call by name parameter.
     */
    def isByNameRef(tree: Tree): Boolean =
      tree.isTerm && tree.hasSymbol &&
      isByNameParamType(tree.symbol.tpe) &&
      !byNameArgs(tree)

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
      appliedType(NonLocalReturnControlClass.typeConstructor, List(argtype))

    /** A hashmap from method symbols to non-local return keys */
    private val nonLocalReturnKeys = perRunCaches.newMap[Symbol, Symbol]()

    /** Return non-local return key for given method */
    private def nonLocalReturnKey(meth: Symbol) =
      nonLocalReturnKeys.getOrElseUpdate(meth, {
        meth.newValue(meth.pos, unit.freshTermName("nonLocalReturnKey"))
          .setFlag (SYNTHETIC)
          .setInfo (ObjectClass.tpe)
      })

    /** Generate a non-local return throw with given return expression from given method.
     *  I.e. for the method's non-local return key, generate:
     *
     *    throw new NonLocalReturnControl(key, expr)
     *  todo: maybe clone a pre-existing exception instead?
     *  (but what to do about exceptions that miss their targets?)
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
     *      case ex: NonLocalReturnControl[_] =>
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
                             AppliedTypeTree(Ident(NonLocalReturnControlClass),
                                             List(Bind(tpnme.WILDCARD,
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
        if (expr hasSymbolWhich (_.isLazy))
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
     *    body = expr match { case P_i if G_i => E_i }_i=1..n
     *  to:
     *
     *    class $anon() extends Object() with PartialFunction[T, R] with ScalaObject {
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
          if (isFunctionType(fun.tpe)) List(abstractFunctionForFunctionType(fun.tpe), SerializableClass.tpe)
          else List(ObjectClass.tpe, fun.tpe, SerializableClass.tpe)

        anonClass setInfo ClassInfoType(parents, new Scope, anonClass)
        val applyMethod = anonClass.newMethod(fun.pos, nme.apply) setFlag FINAL
        applyMethod setInfo MethodType(applyMethod newSyntheticValueParams formals, restpe)
        anonClass.info.decls enter applyMethod
        anonClass.addAnnotation(serialVersionUIDAnnotation)

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
            else Match(substTree(selector.duplicate), (cases map transformCase) :+ defaultCase)
          ))
        }

        val members =
          if (isPartial) List(applyMethodDef, isDefinedAtMethodDef)
          else List(applyMethodDef)

        localTyper.typedPos(fun.pos) {
          Block(
            List(ClassDef(anonClass, NoMods, List(List()), List(List()), members, fun.pos)),
            Typed(
              New(TypeTree(anonClass.tpe), List(List())),
              TypeTree(fun.tpe)))
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
          atPhase(phase.next) {
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
          def getManifest(tp: Type): Tree = {
            val manifestOpt = localTyper.findManifest(tp, false)
            if (!manifestOpt.tree.isEmpty) manifestOpt.tree
            else if (tp.bounds.hi ne tp) getManifest(tp.bounds.hi)
            else localTyper.getManifestTree(tree.pos, tp, false)
          }
          atPhase(phase.next) {
            localTyper.typedPos(pos) {
              Apply(gen.mkAttributedSelect(tree, toArraySym),
                    List(getManifest(tree.tpe.baseType(TraversableClass).typeArgs.head)))
            }
          }
        }

        var suffix: Tree =
          if (treeInfo isWildcardStarArgList args) {
            val Typed(tree, _) = args.last;
            if (isJava)
              if (tree.tpe.typeSymbol == ArrayClass) tree
              else sequenceToArray(tree)
            else
              if (tree.tpe.typeSymbol isSubClass TraversableClass) tree   // @PP: I suspect this should be SeqClass
              else arrayToSequence(tree, varargsElemType)
          }
          else {
            def mkArray = mkArrayValue(args drop (formals.length - 1), varargsElemType)
            if (isJava || inPattern) mkArray
            else if (args.isEmpty) gen.mkNil  // avoid needlessly double-wrapping an empty argument list
            else arrayToSequence(mkArray, varargsElemType)
          }

        atPhase(phase.next) {
          if (isJava && isPrimitiveArray(suffix.tpe) && isArrayOfSymbol(fun.tpe.params.last.tpe, ObjectClass)) {
            suffix = localTyper.typedPos(pos) {
              gen.mkRuntimeCall("toObjectArray", List(suffix))
            }
          }
        }
        args.take(formals.length - 1) :+ (suffix setType formals.last)
      }

      val args1 = if (isVarArgTypes(formals)) transformVarargs(formals.last.typeArgs.head) else args

      (formals, args1).zipped map { (formal, arg) =>
        if (!isByNameParamType(formal)) {
          arg
        } else if (isByNameRef(arg)) {
          byNameArgs += arg
          arg setType functionType(List(), arg.tpe)
        } else {
          if (opt.verboseDebug) {
            val posstr  = arg.pos.source.path + ":" + arg.pos.line
            val permstr = if (fun.isPrivate) "private" else "notprivate"
            log("byname | %s | %s | %s".format(posstr, fun.fullName, permstr))
          }

          val result = localTyper.typed(
            Function(Nil, arg) setPos arg.pos).asInstanceOf[Function]
          new ChangeOwnerTraverser(currentOwner, result.symbol).traverse(arg)
          transformFunction(result)
        }
      }
    }

    /** For removing calls to specially designated methods.
     */
    def elideIntoUnit(tree: Tree): Tree = Literal(()) setPos tree.pos setType UnitClass.tpe
    def isElidable(tree: Tree) = {
      val sym = treeInfo.methPart(tree).symbol
      // XXX settings.noassertions.value temporarily retained to avoid
      // breakage until a reasonable interface is settled upon.
      sym != null && sym.elisionLevel.exists(x => x < settings.elidebelow.value || settings.noassertions.value) && {
        log("Eliding call from " + tree.symbol.owner + " to " + sym + " based on its elision threshold of " + sym.elisionLevel.get)
        true
      }
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
        val sym = currentOwner.newMethod(tree.pos, unit.freshTermName("liftedTree"))
        sym.setInfo(MethodType(List(), tree.tpe))
        new ChangeOwnerTraverser(currentOwner, sym).traverse(tree)
        localTyper.typedPos(tree.pos)(Block(
          List(DefDef(sym, List(Nil), tree)),
          Apply(Ident(sym), Nil)
        ))
      }

      def withInConstructorFlag(inConstructorFlag: Long)(f: => Tree): Tree = {
        val saved = this.inConstructorFlag
        this.inConstructorFlag = inConstructorFlag
        try f
        finally this.inConstructorFlag = saved
      }

      if (isElidable(tree)) elideIntoUnit(tree)
      else tree match {
        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          if (dd.symbol hasAnnotation VarargsClass) saveRepeatedParams(dd)
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
            case _              => sys.error("internal error: UnApply node has wrong symbol")
          })
          treeCopy.UnApply(tree, fn1, args1)

        case Apply(fn, args) =>
          if (fn.symbol == Object_synchronized && shouldBeLiftedAnyway(args.head))
            transform(treeCopy.Apply(tree, fn, List(liftTree(args.head))))
          else
            withNeedLift(true) {
              val formals = fn.tpe.paramTypes
              treeCopy.Apply(tree, transform(fn), transformTrees(transformArgs(tree.pos, fn.symbol, args, formals)))
            }

        case Assign(Select(_, _), _) =>
          withNeedLift(true) { super.transform(tree) }

        case Assign(lhs, _) if lhs.symbol.owner != currentMethod || lhs.symbol.hasFlag(LAZY | ACCESSOR) =>
          withNeedLift(true) { super.transform(tree) }

        case ret @ Return(_) if (isNonLocalReturn(ret)) =>
          withNeedLift(true) { super.transform(ret) }

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
            val tree2 = tree1 setType functionType(Nil, tree1.tpe)
            return {
              if (noApply contains tree2) tree2
              else localTyper.typedPos(tree1.pos)(Apply(Select(tree2, nme.apply), Nil))
            }
          }
          tree1
      }
    } setType {
      assert(tree.tpe != null, tree + " tpe is null")
      uncurryTreeType(tree.tpe)
    }

    def postTransform(tree: Tree): Tree = atPhase(phase.next) {
      def applyUnary(): Tree = {
        def needsParens = tree.symbol.isMethod && !tree.tpe.isInstanceOf[PolyType] // TODO_NMT: verify that the inner tree of a type-apply also gets parens if the whole tree is a polymorphic nullary method application
        def repair = {
          if (!tree.tpe.isInstanceOf[MethodType]) // i.e., it's a NullaryMethodType
            tree.tpe = MethodType(Nil, tree.tpe.resultType) // TODO_NMT: I think the original `tree.tpe` was wrong, since that would set the method's resulttype to PolyType(Nil, restp) instead of restp

          atPos(tree.pos)(Apply(tree, Nil) setType tree.tpe.resultType)
        }

        if (needsParens) repair
        else if (tree.isType) TypeTree(tree.tpe) setPos tree.pos
        else tree
      }

      tree match {
        /* Some uncurry post transformations add members to templates.
         * When inside a template, the following sequence is available:
         * - newMembers
         * Any entry in this sequence will be added into the template
         * once the template transformation has finished.
         *
         * In particular, this case will add:
         * - synthetic Java varargs forwarders for repeated parameters
         */
        case Template(parents, self, body) =>
          localTyper = typer.atOwner(tree, currentClass)
          val tmpl = if (!forMSIL || forMSIL) {
            treeCopy.Template(tree, parents, self, transformTrees(newMembers.toList) ::: body)
          } else super.transform(tree).asInstanceOf[Template]
          newMembers.clear
          tmpl
        case dd @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          val rhs1 = nonLocalReturnKeys.get(tree.symbol) match {
            case None => rhs
            case Some(k) => atPos(rhs.pos)(nonLocalReturnTry(rhs, k, tree.symbol))
          }
          val flatdd = treeCopy.DefDef(tree, mods, name, tparams, List(vparamss.flatten), tpt, rhs1)
          if (dd.symbol hasAnnotation VarargsClass) addJavaVarargsForwarders(dd, flatdd, tree)
          flatdd
        case Try(body, catches, finalizer) =>
          if (catches forall treeInfo.isCatchCase) tree
          else {
            val exname = unit.freshTermName("ex$")
            val cases =
              if ((catches exists treeInfo.isDefaultCase) || (catches.last match {  // bq: handle try { } catch { ... case ex:Throwable => ...}
                    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) if (tpt.tpe =:= ThrowableClass.tpe) =>
                      true
                    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) if (tpt.tpe =:= ThrowableClass.tpe) =>
                      true
                    case _ =>
                      false
                  })) catches
              else catches :+ CaseDef(Ident(nme.WILDCARD), EmptyTree, Throw(Ident(exname)))
            val catchall =
              atPos(tree.pos) {
                CaseDef(
                  Bind(exname, Ident(nme.WILDCARD)),
                  EmptyTree,
                  Match(Ident(exname), cases))
              }
            if (settings.debug.value) log("rewrote try: " + catches + " ==> " + catchall);
            val catches1 = localTyper.typedCases(
              tree, List(catchall), ThrowableClass.tpe, WildcardType)
            treeCopy.Try(tree, body, catches1, finalizer)
          }
        case Apply(Apply(fn, args), args1) =>
          treeCopy.Apply(tree, fn, args ::: args1)
        case Ident(name) =>
          assert(name != tpnme.WILDCARD_STAR)
          applyUnary()
        case Select(_, _) | TypeApply(_, _) =>
          applyUnary()
        case ret @ Return(expr) if (isNonLocalReturn(ret)) =>
          if (settings.debug.value) log("non local return in "+ret.symbol+" from "+currentOwner.enclMethod)
          atPos(ret.pos)(nonLocalReturnThrow(expr, ret.symbol))
        case TypeTree() =>
          tree
        case _ =>
          if (tree.isType) TypeTree(tree.tpe) setPos tree.pos else tree
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
     * varargs forwarder. It then adds the forwarder to the `newMembers` sequence.
     */
    private def addJavaVarargsForwarders(dd: DefDef, flatdd: DefDef, tree: Tree): Unit = {
      if (!repeatedParams.contains(dd.symbol))
        return

      def toSeqType(tp: Type): Type = {
        val arg = elementType(ArrayClass, tp)
        seqType(arg)
      }
      def toArrayType(tp: Type): Type = {
        val arg = elementType(SeqClass, tp)
        // to prevent generation of an `Object` parameter from `Array[T]` parameter later
        // as this would crash the Java compiler which expects an `Object[]` array for varargs
        //   e.g.        def foo[T](a: Int, b: T*)
        //   becomes     def foo[T](a: Int, b: Array[Object])
        //   instead of  def foo[T](a: Int, b: Array[T]) ===> def foo[T](a: Int, b: Object)
        arrayType(
          if (arg.typeSymbol.isTypeParameterOrSkolem) ObjectClass.tpe
          else arg
        )
      }

      val reps          = repeatedParams(dd.symbol)
      val rpsymbols     = reps.map(_.symbol).toSet
      val theTyper      = typer.atOwner(tree, currentClass)
      val flatparams    = flatdd.vparamss.head

      // create the type
      val forwformals = flatparams map {
        case p if rpsymbols(p.symbol) => toArrayType(p.symbol.tpe)
        case p                        => p.symbol.tpe
      }
      val forwresult = dd.symbol.tpe.finalResultType
      val forwformsyms = (forwformals, flatparams).zipped map ((tp, oldparam) =>
        currentClass.newValueParameter(oldparam.symbol.pos, oldparam.name).setInfo(tp)
      )
      def mono = MethodType(forwformsyms, forwresult)
      val forwtype = dd.symbol.tpe match {
        case MethodType(_, _) => mono
        case PolyType(tps, _) => PolyType(tps, mono)
      }

      // create the symbol
      val forwsym = (
        currentClass.newMethod(dd.pos, dd.name)
        . setFlag (VARARGS | SYNTHETIC | flatdd.symbol.flags)
        . setInfo (forwtype)
      )

      // create the tree
      val forwtree = theTyper.typedPos(dd.pos) {
        val locals = (forwsym ARGS, flatparams).zipped map {
          case (_, fp) if !rpsymbols(fp.symbol) => null
          case (argsym, fp)                     =>
            Block(Nil,
              gen.mkCast(
                gen.mkWrapArray(Ident(argsym), elementType(ArrayClass, argsym.tpe)),
                seqType(elementType(SeqClass, fp.symbol.tpe))
              )
            )
        }
        val seqargs = (locals, forwsym ARGS).zipped map {
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

          // add the method to `newMembers`
          newMembers += forwtree
      }
    }
  }

  /** Set of mutable local variables that are free in some inner method. */
  private val freeMutableVars: mutable.Set[Symbol] = new mutable.HashSet

  /** PP: There is apparently some degree of overlap between the CAPTURED
   *  flag and the role being filled here.  I think this is how this was able
   *  to go for so long looking only at DefDef and Ident nodes, as bugs
   *  would only emerge under more complicated conditions such as #3855.
   *  I'll try to figure it all out, but if someone who already knows the
   *  whole story wants to fill it in, that too would be great.
   */
  private val freeLocalsTraverser = new Traverser {
    var currentMethod: Symbol = NoSymbol
    var maybeEscaping = false

    def withEscaping(body: => Unit) {
      val saved = maybeEscaping
      maybeEscaping = true
      try body
      finally maybeEscaping = saved
    }

    override def traverse(tree: Tree) = tree match {
      case DefDef(_, _, _, _, _, _) =>
        val lastMethod = currentMethod
        currentMethod = tree.symbol
        super.traverse(tree)
        currentMethod = lastMethod
      /** A method call with a by-name parameter represents escape. */
      case Apply(fn, args) if fn.symbol.paramss.nonEmpty =>
        traverse(fn)
        (fn.symbol.paramss.head, args).zipped foreach { (param, arg) =>
          if (param.tpe != null && isByNameParamType(param.tpe))
            withEscaping(traverse(arg))
          else
            traverse(arg)
        }
      /** The rhs of a closure represents escape. */
      case Function(vparams, body) =>
        vparams foreach traverse
        withEscaping(traverse(body))

      /** The appearance of an ident outside the method where it was defined or
       *  anytime maybeEscaping is true implies escape.
       */
      case Ident(_) =>
        val sym = tree.symbol
        if (sym.isVariable && sym.owner.isMethod && (maybeEscaping || sym.owner != currentMethod))
          freeMutableVars += sym
      case _ =>
        super.traverse(tree)
    }
  }

}
