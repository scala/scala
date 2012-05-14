/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
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
  import definitions.{ isVarArgsList, isCastSymbol, ThrowableClass, TupleClass }

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
   *  Note that this is not called "isExprSafeToInline" since purity (lack of side-effects)
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
    val depth  = applyDepth(fn)
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
      case Ident(_)         => isVar
      case Select(_, _)     => isVar || isGetter
      case _                =>
        methPart(tree) match {
          case Select(qual, nme.apply)  => qual.tpe.member(nme.update) != NoSymbol
          case _                        => false
        }
    }
  }

  /** Is tree a self constructor call this(...)? I.e. a call to a constructor of the
   *  same object?
   */
  def isSelfConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Ident(nme.CONSTRUCTOR)
       | Select(This(_), nme.CONSTRUCTOR) => true
    case _ => false
  }

  /** Is tree a super constructor call?
   */
  def isSuperConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Select(Super(_, _), nme.CONSTRUCTOR) => true
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
  
  /** Is tree a self or super constructor call? */
  def isSelfOrSuperConstrCall(tree: Tree) = {
    // stripNamedApply for SI-3584: adaptToImplicitMethod in Typers creates a special context
    // for implicit search in constructor calls, adaptToImplicitMethod(isSelfOrConstrCall)
    val tree1 = stripNamedApplyBlock(tree)
    isSelfConstrCall(tree1) || isSuperConstrCall(tree1)
  }

  /** Is tree a variable pattern? */
  def isVarPattern(pat: Tree): Boolean = pat match {
    case x: Ident           => !x.isBackquoted && isVariableName(x.name)
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

  private val reserved = Set[Name](nme.false_, nme.true_, nme.null_)

  /** Is name a variable name? */
  def isVariableName(name: Name): Boolean = {
    val first = name(0)
    ((first.isLower && first.isLetter) || first == '_') && !reserved(name)
  }

  /** Is tree a `this` node which belongs to `enclClass`? */
  def isSelf(tree: Tree, enclClass: Symbol): Boolean = tree match {
    case This(_) => tree.symbol == enclClass
    case _ => false
  }

  /** can this type be a type pattern */
  def mayBeTypePat(tree: Tree): Boolean = tree match {
    case CompoundTypeTree(Template(tps, _, Nil)) => tps exists mayBeTypePat
    case Annotated(_, tp)                        => mayBeTypePat(tp)
    case AppliedTypeTree(constr, args)           => mayBeTypePat(constr) || args.exists(_.isInstanceOf[Bind])
    case SelectFromTypeTree(tp, _)               => mayBeTypePat(tp)
    case _                                       => false
  }

  /** Is this tree comprised of nothing but identifiers,
   *  but possibly in bindings or tuples? For instance
   *
   *    foo @ (bar, (baz, quux))
   *
   *  is a variable pattern; if the structure matches,
   *  then the remainder is inevitable.
   */
  def isVariablePattern(tree: Tree): Boolean = tree match {
    case Bind(name, pat)  => isVariablePattern(pat)
    case Ident(name)      => true
    case Apply(sel, args) =>
      (    isReferenceToScalaMember(sel, TupleClass(args.size).name.toTermName)
        && (args forall isVariablePattern)
      )
    case _ => false
  }

  /** Is this argument node of the form <expr> : _* ?
   */
  def isWildcardStarArg(tree: Tree): Boolean = tree match {
    case Typed(_, Ident(tpnme.WILDCARD_STAR)) => true
    case _                                  => false
  }

  /** If this tree represents a type application (after unwrapping
   *  any applies) the first type argument.  Otherwise, EmptyTree.
   */
  def firstTypeArg(tree: Tree): Tree = tree match {
    case Apply(fn, _)            => firstTypeArg(fn)
    case TypeApply(_, targ :: _) => targ
    case _                       => EmptyTree
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

  /** The method part of an application node
   */
  def methPart(tree: Tree): Tree = tree match {
    case Apply(fn, _)           => methPart(fn)
    case TypeApply(fn, _)       => methPart(fn)
    case AppliedTypeTree(fn, _) => methPart(fn)
    case _                      => tree
  }

  /** The depth of the nested applies: e.g. Apply(Apply(Apply(_, _), _), _)
   *  has depth 3.  Continues through type applications (without counting them.)
   */
  def applyDepth(tree: Tree): Int = tree match {
    case Apply(fn, _)           => 1 + applyDepth(fn)
    case TypeApply(fn, _)       => applyDepth(fn)
    case AppliedTypeTree(fn, _) => applyDepth(fn)
    case _                      => 0
  }
  def firstArgument(tree: Tree): Tree = tree match {
    case Apply(fn, args) =>
      val f = firstArgument(fn)
      if (f == EmptyTree && !args.isEmpty) args.head else f
    case _ =>
      EmptyTree
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
    def containsLeadingPredefImport(defs: List[Tree]): Boolean = defs match {
      case PackageDef(_, defs1) :: _ => containsLeadingPredefImport(defs1)
      case Import(expr, _) :: rest   => isReferenceToPredef(expr) || containsLeadingPredefImport(rest)
      case _                         => false
    }

    // Compilation unit is class or object 'name' in package 'scala'
    def isUnitInScala(tree: Tree, name: Name) = tree match {
      case PackageDef(Ident(nme.scala_), defs) => firstDefinesClassOrObject(defs, name)
      case _                                   => false
    }

    (  isUnitInScala(body, nme.Predef)
    || containsLeadingPredefImport(List(body)))
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


  // domain-specific extractors for reification

  import definitions._

  object TypedOrAnnotated {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case ty @ Typed(_, _) =>
        Some(ty)
      case at @ Annotated(_, _) =>
        Some(at)
      case _ =>
        None
    }
  }

  object TreeSplice {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(splicee, _) if tree.symbol == ExprEval || tree.symbol == ExprValue =>
        Some(splicee)
      case _ =>
        None
    }
  }

  object EvalSplice {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(splicee, _) if tree.symbol == ExprEval =>
        Some(splicee)
      case _ =>
        None
    }
  }

  object ValueSplice {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(splicee, _) if tree.symbol == ExprValue =>
        Some(splicee)
      case _ =>
        None
    }
  }

  object Reified {
    def unapply(tree: Tree): Option[(Tree, List[Tree], Tree)] = tree match {
      case ReifiedTree(reifee, symbolTable, reified, _) =>
        Some(reifee, symbolTable, reified)
      case ReifiedType(reifee, symbolTable, reified) =>
        Some(reifee, symbolTable, reified)
      case _ =>
        None
    }
  }

  object ReifiedTree {
    def unapply(tree: Tree): Option[(Tree, List[Tree], Tree, Tree)] = tree match {
      case reifee @ Block((mrDef @ ValDef(_, _, _, _)) :: symbolTable, Apply(Apply(_, List(tree)), List(Apply(_, tpe :: _)))) if mrDef.name == nme.MIRROR_SHORT =>
        Some(reifee, symbolTable, tree, tpe)
      case _ =>
        None
    }
  }

  object InlineableTreeSplice {
    def unapply(tree: Tree): Option[(Tree, List[Tree], Tree, Tree, Symbol)] = tree match {
      case select @ Select(ReifiedTree(splicee, symbolTable, tree, tpe), _) if select.symbol == ExprEval || select.symbol == ExprValue =>
        Some(splicee, symbolTable, tree, tpe, select.symbol)
      case _ =>
        None
    }
  }

  object InlinedTreeSplice {
    def unapply(tree: Tree): Option[(Tree, List[Tree], Tree, Tree)] = tree match {
      case Select(ReifiedTree(splicee, symbolTable, tree, tpe), name) if name == ExprTree.name =>
        Some(splicee, symbolTable, tree, tpe)
      case _ =>
        None
    }
  }

  object ReifiedType {
    def unapply(tree: Tree): Option[(Tree, List[Tree], Tree)] = tree match {
      case reifee @ Block((mrDef @ ValDef(_, _, _, _)) :: symbolTable, Apply(_, tpe :: _)) if mrDef.name == nme.MIRROR_SHORT =>
        Some(reifee, symbolTable, tpe)
      case _ =>
        None
    }
  }

  object InlinedTypeSplice {
    def unapply(tree: Tree): Option[(Tree, List[Tree], Tree)] = tree match {
      case Select(ReifiedType(splicee, symbolTable, tpe), name) if name == TypeTagTpe.name =>
        Some(splicee, symbolTable, tpe)
      case _ =>
        None
    }
  }

  object FreeDef {
    def unapply(tree: Tree): Option[(Tree, TermName, Tree, Long, String)] = tree match {
      case FreeTermDef(mrRef, name, binding, flags, origin) =>
        Some(mrRef, name, binding, flags, origin)
      case FreeTypeDef(mrRef, name, binding, flags, origin) =>
        Some(mrRef, name, binding, flags, origin)
      case _ =>
        None
    }
  }

  object FreeTermDef {
    lazy val newFreeTermMethod = getMember(getRequiredClass("scala.reflect.api.TreeBuildUtil"), nme.newFreeTerm)

    def unapply(tree: Tree): Option[(Tree, TermName, Tree, Long, String)] = tree match {
      case ValDef(_, name, _, Apply(Select(mrRef @ Ident(_), newFreeTerm), List(_, _, binding, Literal(Constant(flags: Long)), Literal(Constant(origin: String)))))
      if mrRef.name == nme.MIRROR_SHORT && newFreeTerm == newFreeTermMethod.name =>
        Some(mrRef, name, binding, flags, origin)
      case _ =>
        None
    }
  }

  object FreeTypeDef {
    lazy val newFreeExistentialMethod = getMember(getRequiredClass("scala.reflect.api.TreeBuildUtil"), nme.newFreeType)
    lazy val newFreeTypeMethod = getMember(getRequiredClass("scala.reflect.api.TreeBuildUtil"), nme.newFreeExistential)

    def unapply(tree: Tree): Option[(Tree, TermName, Tree, Long, String)] = tree match {
      case ValDef(_, name, _, Apply(Select(mrRef1 @ Ident(_), newFreeType), List(_, _, value, Literal(Constant(flags: Long)), Literal(Constant(origin: String)))))
      if mrRef1.name == nme.MIRROR_SHORT && (newFreeType == newFreeTypeMethod.name || newFreeType == newFreeExistentialMethod.name) =>
        value match {
          case Apply(TypeApply(Select(Select(mrRef2 @ Ident(_), typeTag), apply), List(binding)), List(Literal(Constant(null)), _))
          if mrRef2.name == nme.MIRROR_SHORT && typeTag == nme.TypeTag && apply == nme.apply =>
            Some(mrRef1, name, binding, flags, origin)
          case Apply(TypeApply(Select(mrRef2 @ Ident(_), typeTag), List(binding)), List(Literal(Constant(null)), _))
          if mrRef2.name == nme.MIRROR_SHORT && typeTag == nme.TypeTag =>
            Some(mrRef1, name, binding, flags, origin)
          case _ =>
            throw new Error("unsupported free type def: %s%n%s".format(value, showRaw(value)))
        }
      case _ =>
        None
    }
  }

  object FreeRef {
    def unapply(tree: Tree): Option[(Tree, TermName)] = tree match {
      case Apply(Select(mrRef @ Ident(_), ident), List(Ident(name: TermName))) if ident == nme.Ident && name.startsWith(nme.MIRROR_FREE_PREFIX) =>
        Some(mrRef, name)
      case _ =>
        None
    }
  }

  object TypeRefToFreeType {
    def unapply(tree: Tree): Option[TermName] = tree match {
      case Apply(Select(Select(mrRef @ Ident(_), typeRef), apply), List(Select(_, noSymbol), Ident(freeType: TermName), nil))
      if (mrRef.name == nme.MIRROR_SHORT && typeRef == nme.TypeRef && noSymbol == nme.NoSymbol && freeType.startsWith(nme.MIRROR_FREE_PREFIX)) =>
        Some(freeType)
      case _ =>
        None
    }
  }

  object NestedExpr {
    def unapply(tree: Tree): Option[(Tree, Tree, Tree)] = tree match {
      case Apply(Apply(factory @ Select(expr, apply), List(tree)), List(typetag)) if expr.symbol == ExprModule && apply == nme.apply =>
        Some(factory, tree, typetag)
      case _ =>
        None
    }
  }

  object BoundTerm {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Ident(name) if name.isTermName =>
        Some(tree)
      case This(_) =>
        Some(tree)
      case _ =>
        None
    }
  }

  object BoundType {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(_, name) if name.isTypeName =>
        Some(tree)
      case SelectFromTypeTree(_, name) if name.isTypeName =>
        Some(tree)
      case Ident(name) if name.isTypeName =>
        Some(tree)
      case _ =>
        None
    }
  }
}
