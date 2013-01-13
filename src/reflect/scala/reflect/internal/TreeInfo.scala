/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import Flags._

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo {
  val global: SymbolTable

  import global._
  import definitions.{ isVarArgsList, isCastSymbol, ThrowableClass, TupleClass, MacroContextClass, MacroContextPrefixType }

  /* Does not seem to be used. Not sure what it does anyway.
  def isOwnerDefinition(tree: Tree): Boolean = tree match {
    case PackageDef(_, _)
       | ClassDef(_, _, _, _)
       | ModuleDef(_, _, _)
       | DefDef(_, _, _, _, _, _)
       | Import(_, _) => true
    case _ => false
  }
*/

  // def isDefinition(tree: Tree): Boolean = tree.isDef

  /** Is tree a declaration or type definition?
   */
  def isDeclarationOrTypeDef(tree: Tree): Boolean = tree match {
    case x: ValOrDefDef   => x.rhs eq EmptyTree
    case _                => tree.isInstanceOf[TypeDef]
  }

  /** Is tree legal as a member definition of an interface?
   */
  def isInterfaceMember(tree: Tree): Boolean = tree match {
    case EmptyTree                     => true
    case Import(_, _)                  => true
    case TypeDef(_, _, _, _)           => true
    case DefDef(mods, _, _, _, _, __)  => mods.isDeferred
    case ValDef(mods, _, _, _)         => mods.isDeferred
    case _ => false
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  def isPureDef(tree: Tree): Boolean = tree match {
    case EmptyTree
       | ClassDef(_, _, _, _)
       | TypeDef(_, _, _, _)
       | Import(_, _)
       | DefDef(_, _, _, _, _, _) =>
      true
    case ValDef(mods, _, _, rhs) =>
      !mods.isMutable && isExprSafeToInline(rhs)
    case _ =>
      false
  }

  /** Is tree an expression which can be inlined without affecting program semantics?
   *
   *  Note that this is not called "isExprPure" since purity (lack of side-effects)
   *  is not the litmus test.  References to modules and lazy vals are side-effecting,
   *  both because side-effecting code may be executed and because the first reference
   *  takes a different code path than all to follow; but they are safe to inline
   *  because the expression result from evaluating them is always the same.
   */
  def isExprSafeToInline(tree: Tree): Boolean = tree match {
    case EmptyTree
       | This(_)
       | Super(_, _)
       | Literal(_) =>
      true
    case Ident(_) =>
      tree.symbol.isStable
    // this case is mostly to allow expressions like -5 and +7, but any
    // member of an anyval should be safely pure
    case Select(Literal(const), name) =>
      const.isAnyVal && (const.tpe.member(name) != NoSymbol)
    case Select(qual, _) =>
      tree.symbol.isStable && isExprSafeToInline(qual)
    case TypeApply(fn, _) =>
      isExprSafeToInline(fn)
    case Apply(fn, List()) =>
      /* Note: After uncurry, field accesses are represented as Apply(getter, Nil),
       * so an Apply can also be pure.
       * However, before typing, applications of nullary functional values are also
       * Apply(function, Nil) trees. To prevent them from being treated as pure,
       * we check that the callee is a method. */
      fn.symbol.isMethod && !fn.symbol.isLazy && isExprSafeToInline(fn)
    case Typed(expr, _) =>
      isExprSafeToInline(expr)
    case Block(stats, expr) =>
      (stats forall isPureDef) && isExprSafeToInline(expr)
    case _ =>
      false
  }

  @deprecated("Use isExprSafeToInline instead", "2.10.0")
  def isPureExpr(tree: Tree) = isExprSafeToInline(tree)

  def zipMethodParamsAndArgs(params: List[Symbol], args: List[Tree]): List[(Symbol, Tree)] =
    mapMethodParamsAndArgs(params, args)((param, arg) => ((param, arg)))

  def mapMethodParamsAndArgs[R](params: List[Symbol], args: List[Tree])(f: (Symbol, Tree) => R): List[R] = {
    val b = List.newBuilder[R]
    foreachMethodParamAndArg(params, args)((param, arg) => b += f(param, arg))
    b.result
  }
  def foreachMethodParamAndArg(params: List[Symbol], args: List[Tree])(f: (Symbol, Tree) => Unit): Boolean = {
    val plen   = params.length
    val alen   = args.length
    def fail() = {
      global.debugwarn(
        "Mismatch trying to zip method parameters and argument list:\n" +
        "  params = " + params + "\n" +
        "    args = " + args + "\n"
      )
      false
    }

    if (plen == alen) foreach2(params, args)(f)
    else if (params.isEmpty) return fail
    else if (isVarArgsList(params)) {
      val plenInit = plen - 1
      if (alen == plenInit) {
        if (alen == 0) Nil        // avoid calling mismatched zip
        else foreach2(params.init, args)(f)
      }
      else if (alen < plenInit) return fail
      else {
        foreach2(params.init, args take plenInit)(f)
        val remainingArgs = args drop plenInit
        foreach2(List.fill(remainingArgs.size)(params.last), remainingArgs)(f)
      }
    }
    else return fail

    true
  }

  /**
   * Selects the correct parameter list when there are nested applications.
   * Given Apply(fn, args), args might correspond to any of fn.symbol's parameter
   * lists.  To choose the correct one before uncurry, we have to unwrap any
   * applies: for instance Apply(fn @ Apply(Apply(_, _), _), args) implies args
   * correspond to the third parameter list.
   *
   * The argument fn is the function part of the apply node being considered.
   *
   * Also accounts for varargs.
   */
  private def applyMethodParameters(fn: Tree): List[Symbol] = {
    val depth  = dissectApplied(fn).applyDepth
    // There could be applies which go beyond the parameter list(s),
    // being applied to the result of the method call.
    // !!! Note that this still doesn't seem correct, although it should
    // be closer than what it replaced.
    if (depth < fn.symbol.paramss.size) fn.symbol.paramss(depth)
    else if (fn.symbol.paramss.isEmpty) Nil
    else fn.symbol.paramss.last
  }

  def zipMethodParamsAndArgs(t: Tree): List[(Symbol, Tree)] = t match {
    case Apply(fn, args) => zipMethodParamsAndArgs(applyMethodParameters(fn), args)
    case _               => Nil
  }
  def foreachMethodParamAndArg(t: Tree)(f: (Symbol, Tree) => Unit): Unit = t match {
    case Apply(fn, args) => foreachMethodParamAndArg(applyMethodParameters(fn), args)(f)
    case _               =>
  }

  /** Is symbol potentially a getter of a variable?
   */
  def mayBeVarGetter(sym: Symbol): Boolean = sym.info match {
    case NullaryMethodType(_)              => sym.owner.isClass && !sym.isStable
    case PolyType(_, NullaryMethodType(_)) => sym.owner.isClass && !sym.isStable
    case mt @ MethodType(_, _)             => mt.isImplicit && sym.owner.isClass && !sym.isStable
    case _                                 => false
  }

  /** Is tree a mutable variable, or the getter of a mutable field?
   */
  def isVariableOrGetter(tree: Tree) = {
    def sym       = tree.symbol
    def isVar     = sym.isVariable
    def isGetter  = mayBeVarGetter(sym) && sym.owner.info.member(nme.getterToSetter(sym.name.toTermName)) != NoSymbol

    tree match {
      case Ident(_)                               => isVar
      case Select(_, _)                           => isVar || isGetter
      case Applied(Select(qual, nme.apply), _, _) => qual.tpe.member(nme.update) != NoSymbol
      case _                                      => false
    }
  }

  /** Is tree a self constructor call this(...)? I.e. a call to a constructor of the
   *  same object?
   */
  def isSelfConstrCall(tree: Tree): Boolean = tree match {
    case Applied(Ident(nme.CONSTRUCTOR), _, _) => true
    case Applied(Select(This(_), nme.CONSTRUCTOR), _, _) => true
    case _ => false
  }

  /** Is tree a super constructor call?
   */
  def isSuperConstrCall(tree: Tree): Boolean = tree match {
    case Applied(Select(Super(_, _), nme.CONSTRUCTOR), _, _) => true
    case _ => false
  }

  /**
   * Named arguments can transform a constructor call into a block, e.g.
   *   <init>(b = foo, a = bar)
   * is transformed to
   *   { val x$1 = foo
   *     val x$2 = bar
   *     <init>(x$2, x$1)
   *   }
   */
  def stripNamedApplyBlock(tree: Tree) = tree match {
    case Block(stats, expr) if stats.forall(_.isInstanceOf[ValDef]) =>
      expr
    case _ =>
      tree
  }

  /** Strips layers of `.asInstanceOf[T]` / `_.$asInstanceOf[T]()` from an expression */
  def stripCast(tree: Tree): Tree = tree match {
    case TypeApply(sel @ Select(inner, _), _) if isCastSymbol(sel.symbol) =>
      stripCast(inner)
    case Apply(TypeApply(sel @ Select(inner, _), _), Nil) if isCastSymbol(sel.symbol) =>
      stripCast(inner)
    case t =>
      t
  }

  object StripCast {
    def unapply(tree: Tree): Some[Tree] = Some(stripCast(tree))
  }

  /** Is tree a self or super constructor call? */
  def isSelfOrSuperConstrCall(tree: Tree) = {
    // stripNamedApply for SI-3584: adaptToImplicitMethod in Typers creates a special context
    // for implicit search in constructor calls, adaptToImplicitMethod(isSelfOrConstrCall)
    val tree1 = stripNamedApplyBlock(tree)
    isSelfConstrCall(tree1) || isSuperConstrCall(tree1)
  }

  /**
   * Does this tree represent an irrefutable pattern match
   * in the position `for { <tree> <- expr }` based only
   * on information at the `parser` phase? To qualify, there
   * may be no subtree that will be interpreted as a
   * Stable Identifier Pattern, nor any type tests, even
   * on TupleN. See SI-6968.
   *
   * For instance:
   *
   * {{{
   * (foo @ (bar @ _)) = 0
   * }}}
   *
   * is a not a variable pattern; if only binds names.
   *
   * The following are not variable patterns.
   *
   * {{{
   *   `bar`
   *   Bar
   *   (a, b)
   *   _: T
   * }}}
   *
   * If the pattern is a simple identifier, it is always
   * a variable pattern. For example, the following
   * introduce new bindings:
   *
   * {{{
   * for { X <- xs } yield X
   * for { `backquoted` <- xs } yield `backquoted`
   * }}}
   *
   * Note that this differs from a case clause:
   *
   * {{{
   *   object X
   *   scrut match {
   *      case X =>  // case _ if scrut == X
   *   }
   * }}}
   *
   * Background: [[https://groups.google.com/d/msg/scala-internals/qwa_XOw_7Ks/IktkeTBYqg0J]]
   *
   */
  def isVarPatternDeep(tree: Tree): Boolean = {
    def isVarPatternDeep0(tree: Tree): Boolean = {
      tree match {
        case Bind(name, pat)  => isVarPatternDeep0(pat)
        case Ident(name)      => isVarPattern(tree)
        case _                => false
      }
    }
    tree match {
      case Ident(name) => true
      case _           => isVarPatternDeep0(tree)
    }
  }

  /** Is tree a variable pattern? */
  def isVarPattern(pat: Tree): Boolean = pat match {
    case x: Ident           => !x.isBackquoted && nme.isVariableName(x.name)
    case _                  => false
  }
  def isDeprecatedIdentifier(tree: Tree): Boolean = tree match {
    case x: Ident           => !x.isBackquoted && nme.isDeprecatedIdentifierName(x.name)
    case _                  => false
  }

  /** The first constructor definitions in `stats` */
  def firstConstructor(stats: List[Tree]): Tree = stats find {
    case x: DefDef  => nme.isConstructorName(x.name)
    case _          => false
  } getOrElse EmptyTree

  /** The arguments to the first constructor in `stats`. */
  def firstConstructorArgs(stats: List[Tree]): List[Tree] = firstConstructor(stats) match {
    case DefDef(_, _, _, args :: _, _, _) => args
    case _                                => Nil
  }

  /** The value definitions marked PRESUPER in this statement sequence */
  def preSuperFields(stats: List[Tree]): List[ValDef] =
    stats collect { case vd: ValDef if isEarlyValDef(vd) => vd }

  def hasUntypedPreSuperFields(stats: List[Tree]): Boolean =
    preSuperFields(stats) exists (_.tpt.isEmpty)

  def isEarlyDef(tree: Tree) = tree match {
    case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  def isEarlyValDef(tree: Tree) = tree match {
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  def isEarlyTypeDef(tree: Tree) = tree match {
    case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  /** Is tpt a vararg type of the form T* ? */
  def isRepeatedParamType(tpt: Tree) = tpt match {
    case TypeTree()                                                          => definitions.isRepeatedParamType(tpt.tpe)
    case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS_NAME), _)      => true
    case AppliedTypeTree(Select(_, tpnme.JAVA_REPEATED_PARAM_CLASS_NAME), _) => true
    case _                                                                   => false
  }

  /** The parameter ValDefs of a method definition that have vararg types of the form T*
   */
  def repeatedParams(tree: Tree): List[ValDef] = tree match {
    case DefDef(_, _, _, vparamss, _, _)  => vparamss.flatten filter (vd => isRepeatedParamType(vd.tpt))
    case _                                => Nil
  }

  /** Is tpt a by-name parameter type of the form => T? */
  def isByNameParamType(tpt: Tree) = tpt match {
    case TypeTree()                                                 => definitions.isByNameParamType(tpt.tpe)
    case AppliedTypeTree(Select(_, tpnme.BYNAME_PARAM_CLASS_NAME), _) => true
    case _                                                          => false
  }

  /** Is name a left-associative operator? */
  def isLeftAssoc(operator: Name) = operator.nonEmpty && (operator.endChar != ':')

  /** Is tree a `this` node which belongs to `enclClass`? */
  def isSelf(tree: Tree, enclClass: Symbol): Boolean = tree match {
    case This(_) => tree.symbol == enclClass
    case _ => false
  }

  /** a Match(Typed(_, tpt), _) must be translated into a switch if isSwitchAnnotation(tpt.tpe) */
  def isSwitchAnnotation(tpe: Type) = tpe hasAnnotation definitions.SwitchClass

  /** can this type be a type pattern */
  def mayBeTypePat(tree: Tree): Boolean = tree match {
    case CompoundTypeTree(Template(tps, _, Nil)) => tps exists mayBeTypePat
    case Annotated(_, tp)                        => mayBeTypePat(tp)
    case AppliedTypeTree(constr, args)           => mayBeTypePat(constr) || args.exists(_.isInstanceOf[Bind])
    case SelectFromTypeTree(tp, _)               => mayBeTypePat(tp)
    case _                                       => false
  }

  /** Is this argument node of the form <expr> : _* ?
   */
  def isWildcardStarArg(tree: Tree): Boolean = tree match {
    case Typed(_, Ident(tpnme.WILDCARD_STAR)) => true
    case _                                  => false
  }

  /** If this tree has type parameters, those.  Otherwise Nil.
   */
  def typeParameters(tree: Tree): List[TypeDef] = tree match {
    case DefDef(_, _, tparams, _, _, _) => tparams
    case ClassDef(_, _, tparams, _)     => tparams
    case TypeDef(_, _, tparams, _)      => tparams
    case _                              => Nil
  }

  /** Does this argument list end with an argument of the form <expr> : _* ? */
  def isWildcardStarArgList(trees: List[Tree]) =
    trees.nonEmpty && isWildcardStarArg(trees.last)

  /** Is the argument a wildcard argument of the form `_` or `x @ _`?
   */
  def isWildcardArg(tree: Tree): Boolean = unbind(tree) match {
    case Ident(nme.WILDCARD) => true
    case _                   => false
  }

  /** Is this pattern node a catch-all (wildcard or variable) pattern? */
  def isDefaultCase(cdef: CaseDef) = cdef match {
    case CaseDef(pat, EmptyTree, _) => isWildcardArg(pat)
    case _                          => false
  }

  /** Does this CaseDef catch Throwable? */
  def catchesThrowable(cdef: CaseDef) = catchesAllOf(cdef, ThrowableClass.tpe)

  /** Does this CaseDef catch everything of a certain Type? */
  def catchesAllOf(cdef: CaseDef, threshold: Type) =
    isDefaultCase(cdef) || (cdef.guard.isEmpty && (unbind(cdef.pat) match {
      case Typed(Ident(nme.WILDCARD), tpt)  => (tpt.tpe != null) && (threshold <:< tpt.tpe)
      case _                                => false
    }))

  /** Is this pattern node a catch-all or type-test pattern? */
  def isCatchCase(cdef: CaseDef) = cdef match {
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) =>
      isSimpleThrowable(tpt.tpe)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) =>
      isSimpleThrowable(tpt.tpe)
    case _ =>
      isDefaultCase(cdef)
  }

  private def isSimpleThrowable(tp: Type): Boolean = tp match {
    case TypeRef(pre, sym, args) =>
      (pre == NoPrefix || pre.widen.typeSymbol.isStatic) &&
      (sym isNonBottomSubClass ThrowableClass) &&  /* bq */ !sym.isTrait
    case _ =>
      false
  }

  /* If we have run-time types, and these are used for pattern matching,
     we should replace this  by something like:

      tp match {
        case TypeRef(pre, sym, args) =>
          args.isEmpty && (sym.owner.isPackageClass || isSimple(pre))
        case NoPrefix =>
          true
        case _ =>
          false
      }
*/

  /** Is this case guarded? */
  def isGuardedCase(cdef: CaseDef) = cdef.guard != EmptyTree

  /** Is this pattern node a sequence-valued pattern? */
  def isSequenceValued(tree: Tree): Boolean = unbind(tree) match {
    case Alternative(ts)            => ts exists isSequenceValued
    case ArrayValue(_, _) | Star(_) => true
    case _                          => false
  }

  /** The underlying pattern ignoring any bindings */
  def unbind(x: Tree): Tree = x match {
    case Bind(_, y) => unbind(y)
    case y          => y
  }

  /** Is this tree a Star(_) after removing bindings? */
  def isStar(x: Tree) = unbind(x) match {
    case Star(_)  => true
    case _        => false
  }


  // used in the symbols for labeldefs and valdefs emitted by the pattern matcher
  // tailcalls, cps,... use this flag combination to detect translated matches
  // TODO: move to Flags
  final val SYNTH_CASE_FLAGS  = CASE | SYNTHETIC

  def isSynthCaseSymbol(sym: Symbol) = sym hasAllFlags SYNTH_CASE_FLAGS
  def hasSynthCaseSymbol(t: Tree)    = t.symbol != null && isSynthCaseSymbol(t.symbol)

  def isTraitRef(tree: Tree): Boolean = {
    val sym = if (tree.tpe != null) tree.tpe.typeSymbol else null
    ((sym ne null) && sym.initialize.isTrait)
  }

  /** Applications in Scala can have one of the following shapes:
   *
   *    1) naked core: Ident(_) or Select(_, _) or basically anything else
   *    2) naked core with targs: TypeApply(core, targs) or AppliedTypeTree(core, targs)
   *    3) apply or several applies wrapping a core: Apply(core, _), or Apply(Apply(core, _), _), etc
   *
   *  This class provides different ways to decompose applications and simplifies their analysis.
   *
   *  ***Examples***
   *  (TypeApply in the examples can be replaced with AppliedTypeTree)
   *
   *    Ident(foo):
   *      * callee = Ident(foo)
   *      * core = Ident(foo)
   *      * targs = Nil
   *      * argss = Nil
   *
   *    TypeApply(foo, List(targ1, targ2...))
   *      * callee = TypeApply(foo, List(targ1, targ2...))
   *      * core = foo
   *      * targs = List(targ1, targ2...)
   *      * argss = Nil
   *
   *    Apply(foo, List(arg1, arg2...))
   *      * callee = foo
   *      * core = foo
   *      * targs = Nil
   *      * argss = List(List(arg1, arg2...))
   *
   *    Apply(Apply(foo, List(arg21, arg22, ...)), List(arg11, arg12...))
   *      * callee = foo
   *      * core = foo
   *      * targs = Nil
   *      * argss = List(List(arg11, arg12...), List(arg21, arg22, ...))
   *
   *    Apply(Apply(TypeApply(foo, List(targs1, targs2, ...)), List(arg21, arg22, ...)), List(arg11, arg12...))
   *      * callee = TypeApply(foo, List(targs1, targs2, ...))
   *      * core = foo
   *      * targs = Nil
   *      * argss = List(List(arg11, arg12...), List(arg21, arg22, ...))
   */
  class Applied(val tree: Tree) {
    /** The tree stripped of the possibly nested applications.
     *  The original tree if it's not an application.
     */
    def callee: Tree = {
      def loop(tree: Tree): Tree = tree match {
        case Apply(fn, _) => loop(fn)
        case tree         => tree
      }
      loop(tree)
    }

    /** The `callee` unwrapped from type applications.
     *  The original `callee` if it's not a type application.
     */
    def core: Tree = callee match {
      case TypeApply(fn, _)       => fn
      case AppliedTypeTree(fn, _) => fn
      case tree                   => tree
    }

    /** The type arguments of the `callee`.
     *  `Nil` if the `callee` is not a type application.
     */
    def targs: List[Tree] = callee match {
      case TypeApply(_, args)       => args
      case AppliedTypeTree(_, args) => args
      case _                        => Nil
    }

    /** (Possibly multiple lists of) value arguments of an application.
     *  `Nil` if the `callee` is not an application.
     */
    def argss: List[List[Tree]] = {
      def loop(tree: Tree): List[List[Tree]] = tree match {
        case Apply(fn, args) => loop(fn) :+ args
        case _               => Nil
      }
      loop(tree)
    }

    /** The depth of the nested applies: e.g. Apply(Apply(Apply(_, _), _), _)
     *  has depth 3.  Continues through type applications (without counting them.)
     */
    def applyDepth: Int = {
      def loop(tree: Tree): Int = tree match {
        case Apply(fn, _)           => 1 + loop(fn)
        case TypeApply(fn, _)       => loop(fn)
        case AppliedTypeTree(fn, _) => loop(fn)
        case _                      => 0
      }
      loop(tree)
    }
  }

  /** Returns a wrapper that knows how to destructure and analyze applications.
   */
  def dissectApplied(tree: Tree) = new Applied(tree)

  /** Destructures applications into important subparts described in `Applied` class,
   *  namely into: core, targs and argss (in the specified order).
   *
   *  Trees which are not applications are also accepted. Their callee and core will
   *  be equal to the input, while targs and argss will be Nil.
   *
   *  The provided extractors don't expose all the API of the `Applied` class.
   *  For advanced use, call `dissectApplied` explicitly and use its methods instead of pattern matching.
   */
  object Applied {
    def unapply(applied: Applied): Option[(Tree, List[Tree], List[List[Tree]])] =
      Some((applied.core, applied.targs, applied.argss))

    def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] =
      unapply(dissectApplied(tree))
  }

  /** Does list of trees start with a definition of
   *  a class of module with given name (ignoring imports)
   */
  def firstDefinesClassOrObject(trees: List[Tree], name: Name): Boolean = trees match {
      case Import(_, _) :: xs               => firstDefinesClassOrObject(xs, name)
      case Annotated(_, tree1) :: Nil       => firstDefinesClassOrObject(List(tree1), name)
      case ModuleDef(_, `name`, _) :: Nil   => true
      case ClassDef(_, `name`, _, _) :: Nil => true
      case _                                => false
    }


  /** Is this file the body of a compilation unit which should not
   *  have Predef imported?
   */
  def noPredefImportForUnit(body: Tree) = {
    // Top-level definition whose leading imports include Predef.
    def isLeadingPredefImport(defn: Tree): Boolean = defn match {
      case PackageDef(_, defs1) => defs1 exists isLeadingPredefImport
      case Import(expr, _)      => isReferenceToPredef(expr)
      case _                    => false
    }
    // Compilation unit is class or object 'name' in package 'scala'
    def isUnitInScala(tree: Tree, name: Name) = tree match {
      case PackageDef(Ident(nme.scala_), defs) => firstDefinesClassOrObject(defs, name)
      case _                                   => false
    }

    isUnitInScala(body, nme.Predef) || isLeadingPredefImport(body)
  }

  def isAbsTypeDef(tree: Tree) = tree match {
    case TypeDef(_, _, _, TypeBoundsTree(_, _)) => true
    case TypeDef(_, _, _, rhs) => rhs.tpe.isInstanceOf[TypeBounds]
    case _ => false
  }

  def isAliasTypeDef(tree: Tree) = tree match {
    case TypeDef(_, _, _, _) => !isAbsTypeDef(tree)
    case _ => false
  }

  /** Some handy extractors for spotting trees through the
   *  the haze of irrelevant braces: i.e. Block(Nil, SomeTree)
   *  should not keep us from seeing SomeTree.
   */
  abstract class SeeThroughBlocks[T] {
    protected def unapplyImpl(x: Tree): T
    def unapply(x: Tree): T = x match {
      case Block(Nil, expr)         => unapply(expr)
      case _                        => unapplyImpl(x)
    }
  }
  object IsTrue extends SeeThroughBlocks[Boolean] {
    protected def unapplyImpl(x: Tree): Boolean = x match {
      case Literal(Constant(true)) => true
      case _                       => false
    }
  }
  object IsFalse extends SeeThroughBlocks[Boolean] {
    protected def unapplyImpl(x: Tree): Boolean = x match {
      case Literal(Constant(false)) => true
      case _                        => false
    }
  }
  object IsIf extends SeeThroughBlocks[Option[(Tree, Tree, Tree)]] {
    protected def unapplyImpl(x: Tree) = x match {
      case If(cond, thenp, elsep) => Some((cond, thenp, elsep))
      case _                      => None
    }
  }

  def isApplyDynamicName(name: Name) = (name == nme.updateDynamic) || (name == nme.selectDynamic) || (name == nme.applyDynamic) || (name == nme.applyDynamicNamed)

  class DynamicApplicationExtractor(nameTest: Name => Boolean) {
    def unapply(tree: Tree) = tree match {
      case Apply(TypeApply(Select(qual, oper), _), List(Literal(Constant(name)))) if nameTest(oper) => Some((qual, name))
      case Apply(Select(qual, oper), List(Literal(Constant(name)))) if nameTest(oper) => Some((qual, name))
      case Apply(Ident(oper), List(Literal(Constant(name)))) if nameTest(oper) => Some((EmptyTree, name))
      case _ => None
    }
  }
  object DynamicUpdate extends DynamicApplicationExtractor(_ == nme.updateDynamic)
  object DynamicApplication extends DynamicApplicationExtractor(isApplyDynamicName)
  object DynamicApplicationNamed extends DynamicApplicationExtractor(_ == nme.applyDynamicNamed)

  object MacroImplReference {
    private def refPart(tree: Tree): Tree = tree match {
      case TypeApply(fun, _) => refPart(fun)
      case ref: RefTree => ref
      case _ => EmptyTree
    }

    def unapply(tree: Tree) = refPart(tree) match {
      case ref: RefTree => Some((ref.qualifier.symbol, ref.symbol, dissectApplied(tree).targs))
      case _            => None
    }
  }

  def isNullaryInvocation(tree: Tree): Boolean =
    tree.symbol != null && tree.symbol.isMethod && (tree match {
      case TypeApply(fun, _) => isNullaryInvocation(fun)
      case tree: RefTree => true
      case _ => false
    })
}
