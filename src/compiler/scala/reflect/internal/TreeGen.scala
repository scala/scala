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
  def mkAttributedQualifier(tpe: Type, termSym: Symbol): Tree = tpe match {
    case NoPrefix =>
      EmptyTree
    case ThisType(clazz) =>
      if (clazz.isEffectiveRoot) EmptyTree
      else mkAttributedThis(clazz)
    case SingleType(pre, sym) =>
      applyIfNoArgs(mkAttributedStableRef(pre, sym))
    case TypeRef(pre, sym, args) =>
      if (sym.isRoot) {
        mkAttributedThis(sym)
      } else if (sym.isModuleClass) {
        applyIfNoArgs(mkAttributedRef(pre, sym.sourceModule))
      } else if (sym.isModule || sym.isClass) {
        assert(phase.erasedTypes, tpe)
        mkAttributedThis(sym)
      } else if (sym.isType) {
        assert(termSym != NoSymbol, tpe)
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
      assert(!firstStable.isEmpty, tpe)
      mkAttributedQualifier(firstStable.get)

    case _ =>
      abort("bad qualifier: " + tpe)
  }
  /** If this is a reference to a method with an empty
   *  parameter list, wrap it in an apply.
   */
  private def applyIfNoArgs(qual: Tree) = qual.tpe match {
    case MethodType(Nil, restpe) => Apply(qual, Nil) setType restpe
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

  /** Cast `tree' to type `pt' */
  def mkCast(tree: Tree, pt: Type): Tree = {
    if (settings.debug.value) log("casting " + tree + ":" + tree.tpe + " to " + pt)
    assert(!tree.tpe.isInstanceOf[MethodType], tree)
    assert(!pt.typeSymbol.isPackageClass)
    assert(!pt.typeSymbol.isPackageObjectClass)
    assert(pt eq pt.normalize, tree +" : "+ debugString(pt) +" ~>"+ debugString(pt.normalize)) //@MAT only called during erasure, which already takes care of that
    atPos(tree.pos)(mkAsInstanceOf(tree, pt, false))
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
        if (sym != null && sym.owner.isPackageObjectClass && sym.owner.owner == qual.tpe.typeSymbol) {
          val obj = sym.owner.sourceModule
          Select(qual, nme.PACKAGEkw) setSymbol obj setType singleType(qual.tpe, obj)
        }
        else qual

      val tree = Select(pkgQualifier, sym)
      if (pkgQualifier.tpe == null) tree
      else tree setType (qual.tpe memberType sym)
    }
  }

  private def mkTypeApply(value: Tree, tpe: Type, what: Symbol) =
    Apply(
      TypeApply(
        mkAttributedSelect(value, what),
        List(TypeTree(tpe.normalize))
      ),
      Nil
    )
  /** Builds an instance test with given value and type. */
  def mkIsInstanceOf(value: Tree, tpe: Type, any: Boolean = true): Tree =
    mkTypeApply(value, tpe, (if (any) Any_isInstanceOf else Object_isInstanceOf))

  /** Builds a cast with given value and type. */
  def mkAsInstanceOf(value: Tree, tpe: Type, any: Boolean = true): Tree =
    mkTypeApply(value, tpe, (if (any) Any_asInstanceOf else Object_asInstanceOf))

  /** Cast `tree' to 'pt', unless tpe is a subtype of pt, or pt is Unit.  */
  def maybeMkAsInstanceOf(tree: Tree, pt: Type, tpe: Type, beforeRefChecks: Boolean = false): Tree =
    if ((pt == UnitClass.tpe) || (tpe <:< pt)) {
      log("no need to cast from " + tpe + " to " + pt)
      tree
    } else
      atPos(tree.pos) {
        if (beforeRefChecks)
          TypeApply(mkAttributedSelect(tree, Any_asInstanceOf), List(TypeTree(pt)))
        else
          mkAsInstanceOf(tree, pt)
      }

  def mkClassOf(tp: Type): Tree =
    Literal(Constant(tp)) setType ConstantType(Constant(tp))// ClassType(tp)

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
    val sym = tp.typeSymbol
    val tree =
      if (sym == UnitClass) Literal(())
      else if (sym == BooleanClass) Literal(false)
      else if (isValueClass(sym)) Literal(0)
      else if (NullClass.tpe <:< tp) Literal(null: Any)
      else abort("Cannot determine zero for " + tp)

    tree setType tp
  }

  /** Builds a tuple */
  def mkTuple(elems: List[Tree]): Tree =
    if (elems.isEmpty) Literal(())
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