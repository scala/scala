/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala
package reflect
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
  import definitions.{ isTupleSymbol, isVarArgsList, isCastSymbol, ThrowableClass, TupleClass, MacroContextClass, MacroContextPrefixType, uncheckedStableClass }

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

  /** Is `tree` a path, defined as follows? (Spec: 3.1 Paths)
   *
   * - The empty path ε (which cannot be written explicitly in user programs).
   * - C.this, where C references a class.
   * - p.x where p is a path and x is a stable member of p.
   * - C.super.x or C.super[M].x where C references a class
   *   and x references a stable member of the super class or designated parent class M of C.
   *
   * NOTE: Trees with errors are (mostly) excluded.
   *
   * Path ::= StableId | [id ‘.’] this
   *
   */
  def isPath(tree: Tree, allowVolatile: Boolean): Boolean =
    tree match {
      // Super is not technically a path.
      // However, syntactically, it can only occur nested in a Select.
      // This gives a nicer definition of isStableIdentifier that's equivalent to the spec's.
      // must consider Literal(_) a path for typedSingletonTypeTree
      case EmptyTree | Literal(_) => true
      case This(_) | Super(_, _)  => symOk(tree.symbol)
      case _                      => isStableIdentifier(tree, allowVolatile)
    }

  /** Is `tree` a stable identifier, a path which ends in an identifier?
   *
   * StableId ::= id
   *           | Path ‘.’ id
   *           | [id ’.’] ‘super’ [‘[’ id ‘]’] ‘.’ id
   */
  def isStableIdentifier(tree: Tree, allowVolatile: Boolean): Boolean =
    tree match {
      case i @ Ident(_)    => isStableIdent(i)
      case Select(qual, _) => isStableMemberOf(tree.symbol, qual, allowVolatile) && isPath(qual, allowVolatile)
      case Apply(Select(free @ Ident(_), nme.apply), _) if free.symbol.name endsWith nme.REIFY_FREE_VALUE_SUFFIX =>
        // see a detailed explanation of this trick in `GenSymbols.reifyFreeTerm`
        free.symbol.hasStableFlag && isPath(free, allowVolatile)
      case _               => false
    }

  private def symOk(sym: Symbol) = sym != null && !sym.isError && sym != NoSymbol
  private def typeOk(tp: Type)   =  tp != null && ! tp.isError

  /** Assuming `sym` is a member of `tree`, is it a "stable member"?
   *
   * Stable members are packages or members introduced
   * by object definitions or by value definitions of non-volatile types (§3.6).
   */
  def isStableMemberOf(sym: Symbol, tree: Tree, allowVolatile: Boolean): Boolean = (
    symOk(sym)       && (!sym.isTerm   || (sym.isStable && (allowVolatile || !sym.hasVolatileType))) &&
    typeOk(tree.tpe) && (allowVolatile || !hasVolatileType(tree)) && !definitions.isByNameParamType(tree.tpe)
  )

  private def isStableIdent(tree: Ident): Boolean = (
       symOk(tree.symbol)
    && tree.symbol.isStable
    && !definitions.isByNameParamType(tree.tpe)
    && !tree.symbol.hasVolatileType // TODO SPEC: not required by spec
  )

  /** Is `tree`'s type volatile? (Ignored if its symbol has the @uncheckedStable annotation.)
   */
  def hasVolatileType(tree: Tree): Boolean =
    symOk(tree.symbol) && tree.tpe.isVolatile && !tree.symbol.hasAnnotation(uncheckedStableClass)

  /** Is `tree` either a non-volatile type,
   *  or a path that does not include any of:
   *   - a reference to a mutable variable/field
   *   - a reference to a by-name parameter
   *   - a member selection on a volatile type (Spec: 3.6 Volatile Types)?
   *
   * Such a tree is a suitable target for type selection.
   */
  def admitsTypeSelection(tree: Tree): Boolean = isPath(tree, allowVolatile = false)

  /** Is `tree` admissible as a stable identifier pattern (8.1.5 Stable Identifier Patterns)?
   *
   * We disregard volatility, as it's irrelevant in patterns (SI-6815)
   */
  def isStableIdentifierPattern(tree: Tree): Boolean = isStableIdentifier(tree, allowVolatile = true)

  // TODO SI-5304 tighten this up so we don't elide side effect in module loads
  def isQualifierSafeToElide(tree: Tree): Boolean = isExprSafeToInline(tree)

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
    case Apply(Select(free @ Ident(_), nme.apply), _) if free.symbol.name endsWith nme.REIFY_FREE_VALUE_SUFFIX =>
      // see a detailed explanation of this trick in `GenSymbols.reifyFreeTerm`
      free.symbol.hasStableFlag && isExprSafeToInline(free)
    case Apply(fn, List()) =>
      // Note: After uncurry, field accesses are represented as Apply(getter, Nil),
      // so an Apply can also be pure.
      // However, before typing, applications of nullary functional values are also
      // Apply(function, Nil) trees. To prevent them from being treated as pure,
      // we check that the callee is a method.
      // The callee might also be a Block, which has a null symbol, so we guard against that (SI-7185)
      fn.symbol != null && fn.symbol.isMethod && !fn.symbol.isLazy && isExprSafeToInline(fn)
    case Typed(expr, _) =>
      isExprSafeToInline(expr)
    case Block(stats, expr) =>
      (stats forall isPureDef) && isExprSafeToInline(expr)
    case _ =>
      false
  }

  /** As if the name of the method didn't give it away,
   *  this logic is designed around issuing helpful
   *  warnings and minimizing spurious ones.  That means
   *  don't reuse it for important matters like inlining
   *  decisions.
   */
  def isPureExprForWarningPurposes(tree: Tree) = tree match {
    case EmptyTree | Literal(Constant(())) => false
    case _                                 =>
      def isWarnableRefTree = tree match {
        case t: RefTree => isExprSafeToInline(t.qualifier) && t.symbol != null && t.symbol.isAccessor
        case _          => false
      }
      def isWarnableSymbol = {
        val sym = tree.symbol
        (sym == null) || !(sym.isModule || sym.isLazy || definitions.isByNameParamType(sym.tpe_*)) || {
          debuglog("'Pure' but side-effecting expression in statement position: " + tree)
          false
        }
      }

      (    !tree.isErrorTyped
        && (isExprSafeToInline(tree) || isWarnableRefTree)
        && isWarnableSymbol
      )
  }

  def mapMethodParamsAndArgs[R](params: List[Symbol], args: List[Tree])(f: (Symbol, Tree) => R): List[R] = {
    val b = List.newBuilder[R]
    foreachMethodParamAndArg(params, args)((param, arg) => b += f(param, arg))
    b.result()
  }
  def foreachMethodParamAndArg(params: List[Symbol], args: List[Tree])(f: (Symbol, Tree) => Unit): Boolean = {
    val plen   = params.length
    val alen   = args.length
    def fail() = {
      global.devWarning(
        s"""|Mismatch trying to zip method parameters and argument list:
            |  params = $params
            |    args = $args""".stripMargin)
      false
    }

    if (plen == alen) foreach2(params, args)(f)
    else if (params.isEmpty) return fail()
    else if (isVarArgsList(params)) {
      val plenInit = plen - 1
      if (alen == plenInit) {
        if (alen == 0) Nil        // avoid calling mismatched zip
        else foreach2(params.init, args)(f)
      }
      else if (alen < plenInit) return fail()
      else {
        foreach2(params.init, args take plenInit)(f)
        val remainingArgs = args drop plenInit
        foreach2(List.fill(remainingArgs.size)(params.last), remainingArgs)(f)
      }
    }
    else return fail()

    true
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
    def isGetter  = mayBeVarGetter(sym) && sym.owner.info.member(sym.setterName) != NoSymbol

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
    case Applied(Ident(nme.CONSTRUCTOR), _, _)           => true
    case Applied(Select(This(_), nme.CONSTRUCTOR), _, _) => true
    case _                                               => false
  }

  /** Is tree a super constructor call?
   */
  def isSuperConstrCall(tree: Tree): Boolean = tree match {
    case Applied(Select(Super(_, _), nme.CONSTRUCTOR), _, _) => true
    case _                                                   => false
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

  /** Translates an Assign(_, _) node to AssignOrNamedArg(_, _) if
   *  the lhs is a simple ident. Otherwise returns unchanged.
   */
  def assignmentToMaybeNamedArg(tree: Tree) = tree match {
    case t @ Assign(id: Ident, rhs) => atPos(t.pos)(AssignOrNamedArg(id, rhs))
    case t                          => t
  }

  /** Is name a left-associative operator? */
  def isLeftAssoc(operator: Name) = operator.nonEmpty && (operator.endChar != ':')

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
    case WildcardStarArg(_) => true
    case _                  => false
  }

  object WildcardStarArg {
    def unapply(tree: Typed): Option[Tree] = tree match {
      case Typed(expr, Ident(tpnme.WILDCARD_STAR)) => Some(expr)
      case _                                       => None
    }
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

  /** Is the argument a wildcard star type of the form `_*`?
   */
  def isWildcardStarType(tree: Tree): Boolean = tree match {
    case Ident(tpnme.WILDCARD_STAR) => true
    case _                          => false
  }

  /** Is this pattern node a catch-all (wildcard or variable) pattern? */
  def isDefaultCase(cdef: CaseDef) = cdef match {
    case CaseDef(pat, EmptyTree, _) => isWildcardArg(pat)
    case _                          => false
  }

  private def hasNoSymbol(t: Tree) = t.symbol == null || t.symbol == NoSymbol

  /** If this CaseDef assigns a name to its top-level pattern,
   *  in the form 'expr @ pattern' or 'expr: pattern', returns
   *  the name. Otherwise, nme.NO_NAME.
   *
   *  Note: in the case of Constant patterns such as 'case x @ "" =>',
   *  the pattern matcher eliminates the binding and inlines the constant,
   *  so as far as this method is likely to be able to determine,
   *  the name is NO_NAME.
   */
  def assignedNameOfPattern(cdef: CaseDef): Name = cdef.pat match {
    case Bind(name, _)  => name
    case Ident(name)    => name
    case _              => nme.NO_NAME
  }

  /** Is this pattern node a synthetic catch-all case, added during PartialFuction synthesis before we know
    * whether the user provided cases are exhaustive. */
  def isSyntheticDefaultCase(cdef: CaseDef) = cdef match {
    case CaseDef(Bind(nme.DEFAULT_CASE, _), EmptyTree, _) => true
    case _                                                => false
  }

  /** Does this CaseDef catch Throwable? */
  def catchesThrowable(cdef: CaseDef) = (
    cdef.guard.isEmpty && (unbind(cdef.pat) match {
      case Ident(nme.WILDCARD) => true
      case i@Ident(name)       => hasNoSymbol(i)
      case _                   => false
    })
  )

  /** Is this CaseDef synthetically generated, e.g. by `MatchTranslation.translateTry`? */
  def isSyntheticCase(cdef: CaseDef) = cdef.pat.exists {
    case dt: DefTree => dt.symbol.isSynthetic
    case _           => false
  }

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
          args.isEmpty && (sym.isTopLevel || isSimple(pre))
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

  /**
   * {{{
   * //------------------------ => effectivePatternArity(args)
   * case Extractor(a)          => 1
   * case Extractor(a, b)       => 2
   * case Extractor((a, b))     => 2
   * case Extractor(a @ (b, c)) => 2
   * }}}
   */
  def effectivePatternArity(args: List[Tree]): Int = (args.map(unbind) match {
    case Apply(fun, xs) :: Nil if isTupleSymbol(fun.symbol) => xs
    case xs                                                 => xs
  }).length


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

    override def toString = {
      val tstr = if (targs.isEmpty) "" else targs.mkString("[", ", ", "]")
      val astr = argss map (args => args.mkString("(", ", ", ")")) mkString ""
      s"$core$tstr$astr"
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
    def apply(tree: Tree): Applied = new Applied(tree)

    def unapply(applied: Applied): Option[(Tree, List[Tree], List[List[Tree]])] =
      Some((applied.core, applied.targs, applied.argss))

    def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] =
      unapply(dissectApplied(tree))
  }

  /** Is this file the body of a compilation unit which should not
   *  have Predef imported? This is the case iff the first import in the
   *  unit explicitly refers to Predef.
   */
  def noPredefImportForUnit(body: Tree) = {
    // Top-level definition whose leading imports include Predef.
    def isLeadingPredefImport(defn: Tree): Boolean = defn match {
      case PackageDef(_, defs1) => defs1 exists isLeadingPredefImport
      case Import(expr, _)      => isReferenceToPredef(expr)
      case _                    => false
    }
    isLeadingPredefImport(body)
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
      case ref: RefTree => {
        val qual = ref.qualifier
        val isBundle = definitions.isMacroBundleType(qual.tpe)
        val owner =
          if (isBundle) qual.tpe.typeSymbol
          else {
            val sym = if (qual.hasSymbolField) qual.symbol else NoSymbol
            if (sym.isModule) sym.moduleClass else sym
          }
        Some((isBundle, owner, ref.symbol, dissectApplied(tree).targs))
      }
      case _  => None
    }
  }

  def isNullaryInvocation(tree: Tree): Boolean =
    tree.symbol != null && tree.symbol.isMethod && (tree match {
      case TypeApply(fun, _) => isNullaryInvocation(fun)
      case tree: RefTree => true
      case _ => false
    })

  def isMacroApplication(tree: Tree): Boolean =
    !tree.isDef && tree.symbol != null && tree.symbol.isMacro && !tree.symbol.isErroneous

  def isMacroApplicationOrBlock(tree: Tree): Boolean = tree match {
    case Block(_, expr) => isMacroApplicationOrBlock(expr)
    case tree => isMacroApplication(tree)
  }

  def isNonTrivialMacroApplication(tree: Tree): Boolean =
    isMacroApplication(tree) && dissectApplied(tree).core != tree
}
