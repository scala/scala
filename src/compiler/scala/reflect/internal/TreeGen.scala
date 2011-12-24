package scala.reflect
package internal

abstract class TreeGen {
  val global: SymbolTable

  import global._
  import definitions._

  def rootId(name: Name)          = Select(Ident(nme.ROOTPKG), name)
  def rootScalaDot(name: Name)    = Select(rootId(nme.scala_) setSymbol ScalaPackage, name)
  def scalaDot(name: Name)        = Select(Ident(nme.scala_) setSymbol ScalaPackage, name)
  def scalaAnyRefConstr           = scalaDot(tpnme.AnyRef)
  def scalaUnitConstr             = scalaDot(tpnme.Unit)
  def scalaScalaObjectConstr      = scalaDot(tpnme.ScalaObject)
  def productConstr               = scalaDot(tpnme.Product)
  def serializableConstr          = scalaDot(tpnme.Serializable)

  def scalaFunctionConstr(argtpes: List[Tree], restpe: Tree, abstractFun: Boolean = false): Tree = {
    val cls = if (abstractFun)
      mkAttributedRef(AbstractFunctionClass(argtpes.length))
    else
      mkAttributedRef(FunctionClass(argtpes.length))
    AppliedTypeTree(cls, argtpes :+ restpe)
  }

  /** A creator for method calls, e.g. fn[T1, T2, ...](v1, v2, ...)
   *  There are a number of variations.
   *
   *  @param    receiver    symbol of the method receiver
   *  @param    methodName  name of the method to call
   *  @param    targs       type arguments (if Nil, no TypeApply node will be generated)
   *  @param    args        value arguments
   *  @return               the newly created trees.
   */
  def mkMethodCall(receiver: Symbol, methodName: Name, targs: List[Type], args: List[Tree]): Tree =
    mkMethodCall(Select(mkAttributedRef(receiver), methodName), targs, args)
  def mkMethodCall(method: Symbol, targs: List[Type], args: List[Tree]): Tree =
    mkMethodCall(mkAttributedRef(method), targs, args)
  def mkMethodCall(method: Symbol, args: List[Tree]): Tree =
    mkMethodCall(method, Nil, args)
  def mkMethodCall(target: Tree, args: List[Tree]): Tree =
    mkMethodCall(target, Nil, args)
  def mkMethodCall(receiver: Symbol, methodName: Name, args: List[Tree]): Tree =
    mkMethodCall(receiver, methodName, Nil, args)
  def mkMethodCall(receiver: Tree, method: Symbol, targs: List[Type], args: List[Tree]): Tree =
    mkMethodCall(Select(receiver, method), targs, args)

  def mkMethodCall(target: Tree, targs: List[Type], args: List[Tree]): Tree =
    Apply(mkTypeApply(target, targs map TypeTree), args)

  /** Builds a reference to value whose type is given stable prefix.
   *  The type must be suitable for this.  For example, it
   *  must not be a TypeRef pointing to an abstract type variable.
   */
  def mkAttributedQualifier(tpe: Type): Tree =
    mkAttributedQualifier(tpe, NoSymbol)

  /** Builds a reference to value whose type is given stable prefix.
   *  If the type is unsuitable, e.g. it is a TypeRef for an
   *  abstract type variable, then an Ident will be made using
   *  termSym as the Ident's symbol.  In that case, termSym must
   *  not be NoSymbol.
   */
  def mkAttributedQualifier(tpe: Type, termSym: Symbol): Tree = {
    def failMessage = "mkAttributedQualifier(" + tpe + ", " + termSym + ")"
    tpe match {
      case NoPrefix =>
        EmptyTree
      case ThisType(clazz) =>
        if (clazz.isEffectiveRoot) EmptyTree
        else mkAttributedThis(clazz)
      case SingleType(pre, sym) =>
        mkApplyIfNeeded(mkAttributedStableRef(pre, sym))
      case TypeRef(pre, sym, args) =>
        if (sym.isRoot) {
          mkAttributedThis(sym)
        } else if (sym.isModuleClass) {
          mkApplyIfNeeded(mkAttributedRef(pre, sym.sourceModule))
        } else if (sym.isModule || sym.isClass) {
          assert(phase.erasedTypes, failMessage)
          mkAttributedThis(sym)
        } else if (sym.isType) {
          assert(termSym != NoSymbol, failMessage)
          mkAttributedIdent(termSym) setType tpe
        } else {
          mkAttributedRef(pre, sym)
        }

      case ConstantType(value) =>
        Literal(value) setType tpe

      case AnnotatedType(_, atp, _) =>
        mkAttributedQualifier(atp)

      case RefinedType(parents, _) =>
        // I am unclear whether this is reachable, but
        // the following implementation looks logical -Lex
        val firstStable = parents.find(_.isStable)
        assert(!firstStable.isEmpty, failMessage + " parents = " + parents)
        mkAttributedQualifier(firstStable.get)

      case _ =>
        abort("bad qualifier received: " + failMessage)
    }
  }
  /** If this is a reference to a method with an empty
   *  parameter list, wrap it in an apply.
   */
  def mkApplyIfNeeded(qual: Tree) = qual.tpe match {
    case MethodType(Nil, restpe) => atPos(qual.pos)(Apply(qual, Nil) setType restpe)
    case _                       => qual
  }

  /** Builds a reference to given symbol with given stable prefix. */
  def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
    val qual = mkAttributedQualifier(pre)
    qual match {
      case EmptyTree                                  => mkAttributedIdent(sym)
      case This(clazz) if qual.symbol.isEffectiveRoot => mkAttributedIdent(sym)
      case _                                          => mkAttributedSelect(qual, sym)
    }
  }

  /** Builds a reference to given symbol. */
  def mkAttributedRef(sym: Symbol): Tree =
    if (sym.owner.isClass) mkAttributedRef(sym.owner.thisType, sym)
    else mkAttributedIdent(sym)

  /** Builds an untyped reference to given symbol. */
  def mkUnattributedRef(sym: Symbol): Tree =
    if (sym.owner.isClass) Select(This(sym.owner), sym)
    else Ident(sym)

  /** Replaces tree type with a stable type if possible */
  def stabilize(tree: Tree): Tree = {
    for(tp <- stableTypeFor(tree)) tree.tpe = tp
    tree
  }

  /** Computes stable type for a tree if possible */
  def stableTypeFor(tree: Tree): Option[Type] = tree match {
    case Ident(_) if tree.symbol.isStable =>
      Some(singleType(tree.symbol.owner.thisType, tree.symbol))
    case Select(qual, _) if ((tree.symbol ne null) && (qual.tpe ne null)) && // turned assert into guard for #4064
                            tree.symbol.isStable && qual.tpe.isStable =>
      Some(singleType(qual.tpe, tree.symbol))
    case _ =>
      None
  }

  /** Cast `tree` to type `pt` */
  def mkCast(tree: Tree, pt: Type): Tree = {
    debuglog("casting " + tree + ":" + tree.tpe + " to " + pt + " at phase: " + phase)
    assert(!tree.tpe.isInstanceOf[MethodType], tree)
    assert(!pt.typeSymbol.isPackageClass && !pt.typeSymbol.isPackageObjectClass, pt)
    // called during (at least): typer, uncurry, explicitouter, cleanup.
    // TODO: figure out the truth table for any/wrapInApply
    // - the `any` flag seems to relate to erasure's adaptMember: "x.asInstanceOf[T] becomes x.$asInstanceOf[T]",
    //   where asInstanceOf is Any_asInstanceOf and $asInstanceOf is Object_asInstanceOf
    //   erasure will only unbox the value in a tree made by mkCast if `any && wrapInApply`
    // - the `wrapInApply` flag need not be true if the tree will be adapted to have the empty argument list added before it gets to erasure
    //   in fact, I think it should be false for trees that will be type checked during typer
    assert(pt eq pt.normalize, tree +" : "+ debugString(pt) +" ~>"+ debugString(pt.normalize))
    atPos(tree.pos)(mkAsInstanceOf(tree, pt, any = false, wrapInApply = true))
  }

  /** Builds a reference with stable type to given symbol */
  def mkAttributedStableRef(pre: Type, sym: Symbol): Tree =
    stabilize(mkAttributedRef(pre, sym))

  def mkAttributedStableRef(sym: Symbol): Tree =
    stabilize(mkAttributedRef(sym))

  def mkAttributedThis(sym: Symbol): Tree =
    This(sym.name.toTypeName) setSymbol sym setType sym.thisType

  def mkAttributedIdent(sym: Symbol): Tree =
    Ident(sym.name) setSymbol sym setType sym.tpe

  def mkAttributedSelect(qual: Tree, sym: Symbol): Tree = {
    // Tests involving the repl fail without the .isEmptyPackage condition.
    if (qual.symbol != null && (qual.symbol.isEffectiveRoot || qual.symbol.isEmptyPackage))
      mkAttributedIdent(sym)
    else {
      val pkgQualifier =
        if (sym != null && sym.owner.isPackageObjectClass && sym.effectiveOwner == qual.tpe.typeSymbol) {
          val obj = sym.owner.sourceModule
          Select(qual, nme.PACKAGE) setSymbol obj setType singleType(qual.tpe, obj)
        }
        else qual

      val tree = Select(pkgQualifier, sym)
      if (pkgQualifier.tpe == null) tree
      else tree setType (qual.tpe memberType sym)
    }
  }

  /** Builds a type application node if args.nonEmpty, returns fun otherwise. */
  def mkTypeApply(fun: Tree, targs: List[Tree]): Tree =
    if (targs.isEmpty) fun else TypeApply(fun, targs)
  def mkTypeApply(target: Tree, method: Symbol, targs: List[Type]): Tree =
    mkTypeApply(Select(target, method), targs map TypeTree)
  def mkAttributedTypeApply(target: Tree, method: Symbol, targs: List[Type]): Tree =
    mkTypeApply(mkAttributedSelect(target, method), targs map TypeTree)

  private def mkSingleTypeApply(value: Tree, tpe: Type, what: Symbol, wrapInApply: Boolean) = {
    val tapp = mkAttributedTypeApply(value, what, List(tpe.normalize))
    if (wrapInApply) Apply(tapp, Nil) else tapp
  }
  private def typeTestSymbol(any: Boolean) = if (any) Any_isInstanceOf else Object_isInstanceOf
  private def typeCastSymbol(any: Boolean) = if (any) Any_asInstanceOf else Object_asInstanceOf

  /** Builds an instance test with given value and type. */
  def mkIsInstanceOf(value: Tree, tpe: Type, any: Boolean = true, wrapInApply: Boolean = true): Tree =
    mkSingleTypeApply(value, tpe, typeTestSymbol(any), wrapInApply)

  /** Builds a cast with given value and type. */
  def mkAsInstanceOf(value: Tree, tpe: Type, any: Boolean = true, wrapInApply: Boolean = true): Tree =
    mkSingleTypeApply(value, tpe, typeCastSymbol(any), wrapInApply)

  /** Cast `tree` to `pt`, unless tpe is a subtype of pt, or pt is Unit.  */
  def maybeMkAsInstanceOf(tree: Tree, pt: Type, tpe: Type, beforeRefChecks: Boolean = false): Tree =
    if ((pt == UnitClass.tpe) || (tpe <:< pt)) tree
    else atPos(tree.pos)(mkAsInstanceOf(tree, pt, any = true, wrapInApply = !beforeRefChecks))

  /** Apparently we smuggle a Type around as a Literal(Constant(tp))
   *  and the implementation of Constant#tpe is such that x.tpe becomes
   *  ClassType(value.asInstanceOf[Type]), i.e. java.lang.Class[Type].
   *  Can't find any docs on how/why it's done this way. See ticket
   *  SI-490 for some interesting comments from lauri alanko suggesting
   *  that the type given by classOf[T] is too strong and should be
   *  weakened so as not to suggest that classOf[List[String]] is any
   *  different from classOf[List[Int]].
   *
   *  !!! See deconstMap in Erasure for one bug this encoding has induced:
   *  I would be very surprised if there aren't more.
   */
  def mkClassOf(tp: Type): Tree =
    Literal(Constant(tp)) setType ConstantType(Constant(tp))

  /** Builds a list with given head and tail. */
  def mkNewCons(head: Tree, tail: Tree): Tree =
    New(Apply(mkAttributedRef(ConsClass), List(head, tail)))

  /** Builds a list with given head and tail. */
  def mkNil: Tree = mkAttributedRef(NilModule)

  /** Builds a tree representing an undefined local, as in
   *    var x: T = _
   *  which is appropriate to the given Type.
   */
  def mkZero(tp: Type): Tree = {
    val tree = tp.typeSymbol match {
      case UnitClass    => Literal(Constant())
      case BooleanClass => Literal(Constant(false))
      case FloatClass   => Literal(Constant(0.0f))
      case DoubleClass  => Literal(Constant(0.0d))
      case ByteClass    => Literal(Constant(0.toByte))
      case ShortClass   => Literal(Constant(0.toShort))
      case IntClass     => Literal(Constant(0))
      case LongClass    => Literal(Constant(0L))
      case CharClass    => Literal(Constant(0.toChar))
      case _            => Literal(Constant(null))
    }
    tree setType tp
  }

  def mkZeroContravariantAfterTyper(tp: Type): Tree = {
    // contravariant -- for replacing an argument in a method call
    // must use subtyping, as otherwise we miss types like `Any with Int`
    val tree =
      if      (NullClass.tpe    <:< tp) Literal(Constant(null))
      else if (UnitClass.tpe    <:< tp) Literal(Constant())
      else if (BooleanClass.tpe <:< tp) Literal(Constant(false))
      else if (FloatClass.tpe   <:< tp) Literal(Constant(0.0f))
      else if (DoubleClass.tpe  <:< tp) Literal(Constant(0.0d))
      else if (ByteClass.tpe    <:< tp) Literal(Constant(0.toByte))
      else if (ShortClass.tpe   <:< tp) Literal(Constant(0.toShort))
      else if (IntClass.tpe     <:< tp) Literal(Constant(0))
      else if (LongClass.tpe    <:< tp) Literal(Constant(0L))
      else if (CharClass.tpe    <:< tp) Literal(Constant(0.toChar))
      else mkCast(Literal(Constant(null)), tp)

    tree
  }

  /** Builds a tuple */
  def mkTuple(elems: List[Tree]): Tree =
    if (elems.isEmpty) Literal(Constant())
    else Apply(
      Select(mkAttributedRef(TupleClass(elems.length).caseModule), nme.apply),
      elems)

  // tree1 AND tree2
  def mkAnd(tree1: Tree, tree2: Tree): Tree =
    Apply(Select(tree1, Boolean_and), List(tree2))

  // tree1 OR tree2
  def mkOr(tree1: Tree, tree2: Tree): Tree =
    Apply(Select(tree1, Boolean_or), List(tree2))
}
