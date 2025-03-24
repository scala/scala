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
package reflect
package internal

import Flags._
import scala.annotation.tailrec

abstract class TreeInfo {
  // FIXME: With `global` as a `val`, implementers must use early initializers, which
  //        are deprecated and will not be supported in 3.0. Please change the design,
  //        remove the early initializers from implementers, and then remove the
  //        `@nowarn` annotations from implementers.
  val global: SymbolTable

  import global._
  import definitions.{ isVarArgsList, isCastSymbol, ThrowableClass, uncheckedStableClass, isBlackboxMacroBundleType, isWhiteboxContextType }

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
    case DefDef(mods, _, _, _, _, _)   => mods.isDeferred
    case ValDef(mods, _, _, _)         => mods.isDeferred
    case _ => false
  }

  def isConstructorWithDefault(t: Tree) = t match {
    case DefDef(_, nme.CONSTRUCTOR, _, vparamss, _, _)  => mexists(vparamss)(_.mods.hasDefault)
    case _                                              => false
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
      case i @ Ident(_)    => isStableIdent(i, allowVolatile)
      case Select(qual, _) => isStableMemberOf(tree.symbol, qual, allowVolatile) && isPath(qual, allowVolatile)
      case Apply(Select(free @ Ident(_), nme.apply), _) if free.symbol.name endsWith nme.REIFY_FREE_VALUE_SUFFIX =>
        // see a detailed explanation of this trick in `GenSymbols.reifyFreeTerm`
        free.symbol.hasStableFlag && isPath(free, allowVolatile)
      case Literal(_)      => true // scala/bug#8855
      case _               => false
    }

  private def symOk(sym: Symbol) = sym != null && !sym.isError && sym != NoSymbol
  private def typeOk(tp: Type)   =  tp != null && ! tp.isError

  private def isUncheckedStable(sym: Symbol) = sym.isTerm && sym.hasAnnotation(uncheckedStableClass)

  /** Assuming `sym` is a member of `tree`, is it a "stable member"?
   *
   * Stable members are packages or members introduced
   * by object definitions or by value definitions of non-volatile types (§3.6).
   */
  def isStableMemberOf(sym: Symbol, tree: Tree, allowVolatile: Boolean): Boolean = (
    symOk(sym)       && (!sym.isTerm   || ((sym.isStable || isUncheckedStable(sym)) && (allowVolatile || !sym.hasVolatileType))) &&
    typeOk(tree.tpe) && (allowVolatile || !hasVolatileType(tree)) && !definitions.isByNameParamType(tree.tpe)
  )

  private def isStableIdent(tree: Ident, allowVolatile: Boolean): Boolean = (
       symOk(tree.symbol)
    && (tree.symbol.isStable || isUncheckedStable(tree.symbol))
    && !definitions.isByNameParamType(tree.tpe)
    && !definitions.isByName(tree.symbol)
    && (allowVolatile || !tree.symbol.hasVolatileType) // TODO SPEC: not required by spec
  )

  /** Is `tree`'s type volatile? (Ignored if its symbol has the @uncheckedStable annotation.)
   */
  def hasVolatileType(tree: Tree): Boolean =
    symOk(tree.symbol) && tree.tpe.isVolatile && !isUncheckedStable(tree.symbol)

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
   * We disregard volatility, as it's irrelevant in patterns (scala/bug#6815)
   */
  def isStableIdentifierPattern(tree: Tree): Boolean = isStableIdentifier(tree, allowVolatile = true)

  // TODO scala/bug#5304 tighten this up so we don't elide side effect in module loads
  def isQualifierSafeToElide(tree: Tree): Boolean = isExprSafeToInline(tree)

  /** Is tree an expression which can be inlined without affecting program semantics?
   *
   *  Note that this is not called "isExprPure" since purity (lack of side-effects)
   *  is not the litmus test.  References to modules and lazy vals are side-effecting,
   *  both because side-effecting code may be executed and because the first reference
   *  takes a different code path than all to follow; but they are safe to inline
   *  because the expression result from evaluating them is always the same.
   */
  @tailrec
  final def isExprSafeToInline(tree: Tree): Boolean = tree match {
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
      // The callee might also be a Block, which has a null symbol, so we guard against that (scala/bug#7185)
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
  @tailrec
  final def isPureExprForWarningPurposes(tree: Tree): Boolean = tree match {
    case Typed(expr, _)                    => isPureExprForWarningPurposes(expr)
    case Function(_, _)                    => true
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

  def isFunctionMissingParamType(tree: Tree): Boolean = tree match {
    case Function(vparams, _) => vparams.exists(_.tpt.isEmpty)
    case _ => false
  }

  def isPartialFunctionMissingParamType(tree: Tree): Boolean = tree match {
    case Match(EmptyTree, _) => true
    case _ => false
  }


  /** Is symbol potentially a getter of a variable?
   */
  def mayBeVarGetter(sym: Symbol): Boolean = sym.info match {
    case NullaryMethodType(_)              => sym.owner.isClass && !sym.isStable
    case PolyType(_, NullaryMethodType(_)) => sym.owner.isClass && !sym.isStable
    case PolyType(_, mt @ MethodType(_, _))=> mt.isImplicit && sym.owner.isClass && !sym.isStable
    case mt @ MethodType(_, _)             => mt.isImplicit && sym.owner.isClass && !sym.isStable
    case _                                 => false
  }

  /** Is tree a mutable variable, or the getter of a mutable field?
   */
  def isVariableOrGetter(tree: Tree) = {
    def sym       = tree.symbol
    def isVar     = sym.isVariable

    tree match {
      case Ident(_)                               => isVar
      case Select(qual, _)                        => isVar || mayBeVarGetter(sym) && qual.tpe.member(sym.setterName) != NoSymbol
      case Applied(Select(qual, nme.apply), _, _) => qual.tpe.member(nme.update) != NoSymbol
      case _                                      => false
    }
  }


  // No field for these vals, which means the ValDef carries the symbol of the getter (and not the field symbol)
  //   - abstract vals have no value we could store (until they become concrete, potentially)
  //   - lazy vals: the ValDef carries the symbol of the lazy accessor.
  //     The sausage factory will spew out the inner workings during the fields phase (actual bitmaps won't follow
  //     until lazyvals & mixins, though we should move this stuff from mixins to lazyvals now that fields takes care of mixing in lazy vals)
  //   - concrete vals in traits don't yield a field here either (their getter's RHS has the initial value)
  //     Constructors will move the assignment to the constructor, abstracting over the field using the field setter,
  //     and Fields will add a field to the class that mixes in the trait, implementing the accessors in terms of it
  //
  // The following case does receive a field symbol (until it's eliminated during the fields phase):
  //   - a concrete val with a statically known value (ConstantType)
  //     performs its side effect according to lazy/strict semantics, but doesn't need to store its value
  //     each access will "evaluate" the RHS (a literal) again
  //
  // We would like to avoid emitting unnecessary fields, but the required knowledge isn't available until after typer.
  // The only way to avoid emitting & suppressing, is to not emit at all until we are sure to need the field, as dotty does.
  def noFieldFor(vd: ValDef, owner: Symbol) = vd.mods.isDeferred || vd.mods.isLazy || (owner.isTrait && !vd.mods.hasFlag(PRESUPER))


  def isDefaultGetter(tree: Tree) = {
    tree.symbol != null && tree.symbol.isDefaultGetter
  }

  /** Is tree a self constructor call this(...)? I.e. a call to a constructor of the
   *  same object?
   */
  def isSelfConstrCall(tree: Tree): Boolean = dissectCore(tree) match {
    case Ident(nme.CONSTRUCTOR)           => true
    case Select(This(_), nme.CONSTRUCTOR) => true
    case _                                => false
  }

  /** Is tree a super constructor call?
   */
  def isSuperConstrCall(tree: Tree): Boolean = dissectCore(tree) match {
    case Select(Super(_, _), nme.CONSTRUCTOR) => true
    case _                                    => false
  }

  /** Is tree an application with result `this.type`?
   *  Accept `b.addOne(x)` and also `xs(i) += x`
   *  where the op is an assignment operator.
   */
  def isThisTypeResult(tree: Tree): Boolean = tree match {
    case Applied(fun @ Select(receiver, op), _, argss) =>
      tree.tpe match {
        case ThisType(sym) =>
          sym == receiver.symbol
        case SingleType(_, sym) =>
          sym == receiver.symbol || argss.exists(_.exists(sym == _.symbol))
        case _ =>
          def checkSingle(sym: Symbol): Boolean =
            (sym == receiver.symbol) || {
              receiver match {
                case Apply(_, _) => Precedence(op.decoded).level == 0         // xs(i) += x
                case _ => receiver.symbol != null &&
                  (receiver.symbol.isGetter || receiver.symbol.isField)       // xs.addOne(x) for var xs
              }
            }
          @tailrec def loop(mt: Type): Boolean = mt match {
            case MethodType(_, restpe) =>
              restpe match {
                case ThisType(sym) => checkSingle(sym)
                case SingleType(_, sym) => checkSingle(sym)
                case _ => loop(restpe)
              }
            case PolyType(_, restpe) => loop(restpe)
            case _ => false
          }
          fun.symbol != null && loop(fun.symbol.info)
      }
    case _ =>
      tree.tpe.isInstanceOf[ThisType]
  }

  /**
   * Named arguments can transform a constructor call into a block, e.g.
   *   <init>(b = foo, a = bar)
   * is transformed to
   *   { val x\$1 = foo
   *     val x\$2 = bar
   *     <init>(x\$2, x\$1)
   *   }
   */
  def stripNamedApplyBlock(tree: Tree) = tree match {
    case Block(stats, expr) if stats.forall(_.isInstanceOf[ValDef]) =>
      expr
    case _ =>
      tree
  }

  /** Strips layers of `.asInstanceOf[T]` / `_.\$asInstanceOf[T]()` from an expression */
  @tailrec
  final def stripCast(tree: Tree): Tree = tree match {
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
    // stripNamedApply for scala/bug#3584: adaptToImplicitMethod in Typers creates a special context
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
   * on TupleN. See scala/bug#6968.
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
    @tailrec
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

  def isLiteralString(t: Tree): Boolean = t match {
    case Literal(Constant(_: String)) => true
    case _ => false
  }

  /** Does the tree have a structure similar to typechecked trees? */
  private[internal] def detectTypecheckedTree(tree: Tree) =
    tree.hasExistingSymbol || tree.exists {
      case dd: DefDef => dd.mods.hasAccessorFlag || dd.mods.isSynthetic // for untypechecked trees
      case md: MemberDef => md.hasExistingSymbol
      case _ => false
    }

  /** Recover template body to parsed state */
  private[internal] def untypecheckedTemplBody(templ: Template) =
    untypecheckedTreeBody(templ, templ.body)

  /** Recover block body to parsed state */
  private[internal] def untypecheckedBlockBody(block: Block) =
    untypecheckedTreeBody(block, block.stats)

  /** Recover tree body to parsed state */
  private[internal] def untypecheckedTreeBody(tree: Tree, tbody: List[Tree]) = {
    def filterBody(body: List[Tree]) = body filter {
      case _: ValDef | _: TypeDef => true
      // keep valdef or getter for val/var
      case dd: DefDef if dd.mods.hasAccessorFlag => !nme.isSetterName(dd.name) && !tbody.exists {
        case vd: ValDef => dd.name == vd.name.dropLocal
        case _ => false
      }
      case md: MemberDef => !md.mods.isSynthetic
      case _ => true
    }

    def lazyValDefRhs(body: Tree) =
      body match {
        case Block(List(Assign(_, rhs)), _) => rhs
        case _ => body
      }

    def recoverBody(body: List[Tree]) = body map {
      case vd @ ValDef(vmods, vname, _, vrhs) if nme.isLocalName(vname) =>
        tbody.collectFirst {
          case DefDef(dmods, dname, _, _, _, drhs) if dname == vname.dropLocal =>
            // get access flags from DefDef
            val defDefMask = Flags.AccessFlags | OVERRIDE | IMPLICIT | DEFERRED
            val vdMods = (vmods &~ defDefMask) | (dmods & defDefMask).flags
            // for most cases lazy body should be taken from accessor DefDef
            val vdRhs = if (vmods.isLazy) lazyValDefRhs(drhs) else vrhs
            copyValDef(vd)(mods = vdMods, name = dname, rhs = vdRhs)
        }.getOrElse(vd)
      // for abstract and some lazy val/vars
      case DefDef(mods, name, _, _, tpt, rhs) if mods.hasAccessorFlag =>
        // transform getter mods to field
        val vdMods = (if (!mods.hasStableFlag) mods | Flags.MUTABLE else mods &~ Flags.STABLE) &~ Flags.ACCESSOR
        ValDef(vdMods, name, tpt, rhs)
      case tr => tr
    }

    if (detectTypecheckedTree(tree)) {
      recoverBody(filterBody(tbody))
    } else tbody
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

  /** The modifiers of the first constructor in `stats`. */
  def firstConstructorMods(stats: List[Tree]): Modifiers = firstConstructor(stats) match {
    case DefDef(mods, _, _, _, _, _) => mods
    case _                           => Modifiers()
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

  /** Is tpt a vararg type of the form T* ? */
  def isRepeatedParamType(tpt: Tree) = tpt match {
    case TypeTree()                                                          => definitions.isRepeatedParamType(tpt.tpe)
    case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS_NAME), _)      => true
    case AppliedTypeTree(Select(_, tpnme.JAVA_REPEATED_PARAM_CLASS_NAME), _) => true
    case _                                                                   => false
  }

  /** Is tpt a by-name parameter type of the form => T? */
  def isByNameParamType(tpt: Tree) = tpt match {
    case TypeTree()                                                 => definitions.isByNameParamType(tpt.tpe)
    case AppliedTypeTree(Select(_, tpnme.BYNAME_PARAM_CLASS_NAME), _) => true
    case _                                                          => false
  }

  /** Translates an Assign(_, _) node to NamedArg(_, _) if
   *  the lhs is a simple ident. Otherwise returns unchanged.
   */
  def assignmentToMaybeNamedArg(tree: Tree) = tree match {
    case t @ Assign(id: Ident, rhs) => atPos(t.pos)(NamedArg(id, rhs))
    case t                          => t
  }

  /** a Match(Typed(_, tpt), _) must be translated into a switch if isSwitchAnnotation(tpt.tpe) */
  def isSwitchAnnotation(tpe: Type) = tpe hasAnnotation definitions.SwitchClass

  /** can this type be a type pattern */
  final def mayBeTypePat(tree: Tree): Boolean = tree match {
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
    def unapply(tree: Tree): Option[Tree] = tree match {
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

  /** Is this pattern node a synthetic catch-all case, added during PartialFunction synthesis before we know
    * whether the user provided cases are exhaustive. */
  def isSyntheticDefaultCase(cdef: CaseDef) = cdef match {
    case CaseDef(Bind(nme.DEFAULT_CASE, _), EmptyTree, _) => true
    case _                                                => false
  }

  /** Does this CaseDef catch Throwable? */
  def catchesThrowable(cdef: CaseDef) = (
    cdef.guard.isEmpty && (unbind(cdef.pat) match {
      case Ident(nme.WILDCARD) => true
      case i@Ident(_)          => hasNoSymbol(i)
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
  @tailrec
  final def unbind(x: Tree): Tree = x match {
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
  def effectivePatternArity(args: List[Tree]): Int = flattenedPatternArgs(args).length

  def flattenedPatternArgs(args: List[Tree]): List[Tree] = args map unbind match {
    case build.SyntacticTuple(xs) :: Nil => xs
    case xs                              => xs
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

  def hasExplicitUnit(tree: Tree): Boolean =
    explicitlyUnit(tree) || {
      tree match {
        case Apply(f, _)           => hasExplicitUnit(f)
        case TypeApply(f, _)       => hasExplicitUnit(f)
        case AppliedTypeTree(f, _) => hasExplicitUnit(f)
        case _                     => false
      }
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
  final class Applied(val tree: Tree) {
    /** The tree stripped of the possibly nested applications.
     *  The original tree if it's not an application.
     */
    def callee: Tree = {
      @tailrec
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
      case callee                 => callee
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
  }

  /** Returns a wrapper that knows how to destructure and analyze applications.
   */
  final def dissectApplied(tree: Tree) = new Applied(tree)
  /** Equivalent to dissectApplied(tree).core, but more efficient */
  @scala.annotation.tailrec
  final def dissectCore(tree: Tree): Tree = tree match {
    case TypeApply(fun, _) =>
      dissectCore(fun)
    case Apply(fun, _) =>
      dissectCore(fun)
    case t =>
      t
  }


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

    def unapply(applied: Applied): Some[(Tree, List[Tree], List[List[Tree]])] =
      Some((applied.core, applied.targs, applied.argss))

    def unapply(tree: Tree): Some[(Tree, List[Tree], List[List[Tree]])] =
      unapply(dissectApplied(tree))
  }

  /**
   * Deconstructs an application into fun (typically a Select), targs and argss.
   * Unlike `Applied`, only matches if the tree is actually an application (Apply and / or TypeApply).
   */
  object Application {
    def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] = {
      val ap = new Applied(tree)
      val core = ap.core
      if (core eq tree) None
      else Some((core, ap.targs, ap.argss))
    }
  }

  /** Does list of trees start with a definition of
   *  a class or module with given name (ignoring imports)
   */
  @tailrec
  final def firstDefinesClassOrObject(trees: List[Tree], name: Name): Boolean = trees match {
    case Import(_, _) :: xs             => firstDefinesClassOrObject(xs, name)
    case Annotated(_, tree1) :: _       => firstDefinesClassOrObject(List(tree1), name)
    case ModuleDef(_, `name`, _) :: _   => true
    case ClassDef(_, `name`, _, _) :: _ => true
    case _                              => false
  }

  /** Locates the synthetic Apply node corresponding to an extractor's call to
   *  unapply (unwrapping nested Applies) and returns the fun part of that Apply.
   */
  object Unapplied {
    // Duplicated with `spliceApply`
    @tailrec
    def unapply(tree: Tree): Option[Tree] = tree match {
      // scala/bug#7868 Admit Select() to account for numeric widening, e.g. <unapplySelector>.toInt
      case Apply(fun, (Ident(nme.SELECTOR_DUMMY)| Select(Ident(nme.SELECTOR_DUMMY), _)) :: Nil)
                         => Some(fun)
      case Apply(fun, _) => unapply(fun)
      case _             => None
    }
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

  def isApplyDynamicName(name: Name) = (name == nme.updateDynamic) || (name == nme.selectDynamic) || (name == nme.applyDynamic) || (name == nme.applyDynamicNamed)

  private object LiteralNameOrAdapted {
    def unapply(tree: Tree) = tree match {
      case Literal(Constant(name))                 => Some(name)
      case Apply(_, List(Literal(Constant(name)))) => Some(name)
      case _                                       => None
    }
  }
  class DynamicApplicationExtractor(nameTest: Name => Boolean) {
    def unapply(tree: Tree) = tree match {
      case Apply(TypeApply(Select(qual, oper), _), List(LiteralNameOrAdapted(name))) if nameTest(oper) => Some((qual, name))
      case Apply(Select(qual, oper), List(LiteralNameOrAdapted(name))) if nameTest(oper)               => Some((qual, name))
      case Apply(Ident(oper), List(LiteralNameOrAdapted(name))) if nameTest(oper)                      => Some((EmptyTree, name))
      case _                                                                                           => None
    }
  }
  object DynamicUpdate extends DynamicApplicationExtractor(_ == nme.updateDynamic)
  object DynamicApplication extends DynamicApplicationExtractor(isApplyDynamicName)
  object DynamicApplicationNamed extends DynamicApplicationExtractor(_ == nme.applyDynamicNamed)

  object MacroImplReference {
    @tailrec
    private def refPart(tree: Tree): Tree = tree match {
      case TypeApply(fun, _) => refPart(fun)
      case ref: RefTree => ref
      case _ => EmptyTree
    }

    def unapply(tree: Tree) = refPart(tree) match {
      case ref: RefTree => {
        val qual = ref.qualifier
        val isBundle = definitions.isMacroBundleType(qual.tpe)
        val isBlackbox =
          if (isBundle) isBlackboxMacroBundleType(qual.tpe)
          else ref.symbol.paramss match {
            case (c :: Nil) :: _ if isWhiteboxContextType(c.info) => false
            case _ => true
          }
        val owner =
          if (isBundle) qual.tpe.typeSymbol
          else {
            val qualSym = if (qual.hasSymbolField) qual.symbol else NoSymbol
            if (qualSym.isModule) qualSym.moduleClass else qualSym
          }
        Some((isBundle, isBlackbox, owner, ref.symbol, dissectApplied(tree).targs))
      }
      case _  => None
    }
  }

  @tailrec
  final def isNullaryInvocation(tree: Tree): Boolean =
    tree.symbol != null && tree.symbol.isMethod && (tree match {
      case TypeApply(fun, _) => isNullaryInvocation(fun)
      case _: RefTree => true
      case _ => false
    })

  def isMacroApplication(tree: Tree): Boolean = !tree.isDef && {
    val sym = tree.symbol
    sym != null && sym.isTermMacro && !sym.isErroneous
  }

  @tailrec
  final def isMacroApplicationOrBlock(tree: Tree): Boolean = tree match {
    case Block(_, expr) => isMacroApplicationOrBlock(expr)
    case tree => isMacroApplication(tree)
  }
}

// imported from scalamacros/paradise
trait MacroAnnotionTreeInfo { self: TreeInfo =>
  import global._
  import definitions._
  import build.{SyntacticClassDef, SyntacticTraitDef}

  def primaryConstructorArity(tree: ClassDef): Int = treeInfo.firstConstructor(tree.impl.body) match {
    case DefDef(_, _, _, params :: _, _, _) => params.length
    case x                                  => throw new MatchError(x)
  }

  def anyConstructorHasDefault(tree: ClassDef): Boolean = tree.impl.body exists {
    case DefDef(_, nme.CONSTRUCTOR, _, paramss, _, _) => mexists(paramss)(_.mods.hasDefault)
    case _                                            => false
  }

  def isMacroAnnotation(tree: ClassDef): Boolean = {
    val clazz = tree.symbol
    def isAnnotation = clazz isNonBottomSubClass AnnotationClass
    def hasMacroTransformMethod = clazz.info.member(nme.macroTransform) != NoSymbol
    clazz != null && isAnnotation && hasMacroTransformMethod
  }

  case class AnnotationZipper(annotation: Tree, annottee: Tree, owner: Tree)

  // TODO: no immediate idea how to write this in a sane way
  def getAnnotationZippers(tree: Tree): List[AnnotationZipper] = {
    def loop[T <: Tree](tree: T, deep: Boolean): List[AnnotationZipper] = tree match {
      case SyntacticClassDef(mods, name, tparams, constrMods, vparamss, earlyDefs, parents, selfdef, body) =>
        val czippers = mods.annotations.map { ann =>
          val mods1 = mods.mapAnnotations(_ diff List(ann))
          val annottee = PatchedSyntacticClassDef(mods1, name, tparams, constrMods, vparamss, earlyDefs, parents, selfdef, body)
          AnnotationZipper(ann, annottee, annottee)
        }
        if (!deep) czippers
        else {
          val tzippers = for {
            tparam <- tparams
            AnnotationZipper(ann, tparam1: TypeDef, _) <- loop(tparam, deep = false)
            tparams1 = tparams.updated(tparams.indexOf(tparam), tparam1)
          } yield AnnotationZipper(ann, tparam1, PatchedSyntacticClassDef(mods, name, tparams1, constrMods, vparamss, earlyDefs, parents, selfdef, body))
          val vzippers = for {
            vparams <- vparamss
            vparam <- vparams
            AnnotationZipper(ann, vparam1: ValDef, _) <- loop(vparam, deep = false)
            vparams1 = vparams.updated(vparams.indexOf(vparam), vparam1)
            vparamss1 = vparamss.updated(vparamss.indexOf(vparams), vparams1)
          } yield AnnotationZipper(ann, vparam1, PatchedSyntacticClassDef(mods, name, tparams, constrMods, vparamss1, earlyDefs, parents, selfdef, body))
          czippers ++ tzippers ++ vzippers
        }
      case SyntacticTraitDef(mods, name@_, tparams, earlyDefs@_, parents@_, selfdef@_, body@_) =>
        val tdef = tree.asInstanceOf[ClassDef]
        val czippers = mods.annotations.map(ann => {
          val annottee = tdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
          AnnotationZipper(ann, annottee, annottee)
        })
        if (!deep) czippers
        else {
          val tzippers = for {
            tparam <- tparams
            AnnotationZipper(ann, tparam1: TypeDef, _) <- loop(tparam, deep = false)
            tparams1 = tparams.updated(tparams.indexOf(tparam), tparam1)
          } yield AnnotationZipper(ann, tparam1, tdef.copy(tparams = tparams1))
          czippers ++ tzippers
        }
      case mdef @ ModuleDef(mods, _, _) =>
        mods.annotations.map(ann => {
          val annottee = mdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
          AnnotationZipper(ann, annottee, annottee)
        })
      case ddef @ DefDef(mods, _, tparams, vparamss, _, _) =>
        val dzippers = mods.annotations.map(ann => {
          val annottee = ddef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
          AnnotationZipper(ann, annottee, annottee)
        })
        if (!deep) dzippers
        else {
          val tzippers = for {
            tparam <- tparams
            AnnotationZipper(ann, tparam1: TypeDef, _) <- loop(tparam, deep = false)
            tparams1 = tparams.updated(tparams.indexOf(tparam), tparam1)
          } yield AnnotationZipper(ann, tparam1, ddef.copy(tparams = tparams1))
          val vzippers = for {
            vparams <- vparamss
            vparam <- vparams
            AnnotationZipper(ann, vparam1: ValDef, _) <- loop(vparam, deep = false)
            vparams1 = vparams.updated(vparams.indexOf(vparam), vparam1)
            vparamss1 = vparamss.updated(vparamss.indexOf(vparams), vparams1)
          } yield AnnotationZipper(ann, vparam1, ddef.copy(vparamss = vparamss1))
          dzippers ++ tzippers ++ vzippers
        }
      case vdef @ ValDef(mods, _, _, _) =>
        mods.annotations.map(ann => {
          val annottee = vdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
          AnnotationZipper(ann, annottee, annottee)
        })
      case tdef @ TypeDef(mods, _, tparams, _) =>
        val tzippers = mods.annotations.map(ann => {
          val annottee = tdef.copy(mods = mods.mapAnnotations(_ diff List(ann)))
          AnnotationZipper(ann, annottee, annottee)
        })
        if (!deep) tzippers
        else {
          val ttzippers = for {
            tparam <- tparams
            AnnotationZipper(ann, tparam1: TypeDef, _) <- loop(tparam, deep = false)
            tparams1 = tparams.updated(tparams.indexOf(tparam), tparam1)
          } yield AnnotationZipper(ann, tparam1, tdef.copy(tparams = tparams1))
          tzippers ++ ttzippers
        }
      case _ =>
        Nil
    }
    loop(tree, deep = true)
  }

  private object PatchedSyntacticClassDef {
    def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
              constrMods: Modifiers, vparamss: List[List[Tree]],
              earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef = {
      // NOTE: works around SI-8771 and hopefully fixes https://github.com/scalamacros/paradise/issues/53 for good
      SyntacticClassDef(mods, name, tparams, constrMods, vparamss.map(_.map(_.duplicate)), earlyDefs, parents, selfType, body)
    }
  }

  // Return a pair consisting of (all statements up to and including superclass and trait constr calls, rest)
  final def splitAtSuper(stats: List[Tree], classOnly: Boolean): (List[Tree], List[Tree]) = {
    @tailrec
    def isConstr(tree: Tree): Boolean = tree match {
      case Block(_, expr) =>
        isConstr(expr) // scala/bug#6481 account for named argument blocks
      case Apply(Select(New(_), _), _) =>
        false // scala/bug#11736 don't treat `new X` statements as super calls
      case Apply(fun, _) =>
        (fun.symbol ne null) && (if (classOnly) fun.symbol.isClassConstructor else fun.symbol.isConstructor)
      case _ =>
        false
    }
    val (pre, rest0)       = stats span (!isConstr(_))
    val (supercalls, rest) = rest0 span (isConstr(_))
    (pre ::: supercalls, rest)
  }

}
