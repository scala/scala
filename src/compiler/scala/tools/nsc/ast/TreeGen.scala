/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.collection.mutable.ListBuffer
import symtab.Flags._
import symtab.SymbolTable

/** XXX to resolve: TreeGen only assumes global is a SymbolTable, but
 *  TreeDSL at the moment expects a Global.  Can we get by with SymbolTable?
 */
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
    case This(_) if tree.symbol != null && !tree.symbol.isError =>
      Some(ThisType(tree.symbol))
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

  def mkCheckInit(tree: Tree): Tree = {
    val tpe =
      if (tree.tpe != null || !tree.hasSymbol) tree.tpe
      else tree.symbol.tpe

    if (!global.phase.erasedTypes && settings.warnSelectNullable.value &&
        tpe <:< NotNullClass.tpe && !tpe.isNotNull)
      mkRuntimeCall(nme.checkInitialized, List(tree))
    else
      tree
  }

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
      case UnitClass    => Literal(())
      case BooleanClass => Literal(false)
      case FloatClass   => Literal(0.0f)
      case DoubleClass  => Literal(0.0d)
      case ByteClass    => Literal(0.toByte)
      case ShortClass   => Literal(0.toShort)
      case IntClass     => Literal(0)
      case LongClass    => Literal(0L)
      case CharClass    => Literal(0.toChar)
      case _            =>
        if (NullClass.tpe <:< tp) Literal(null: Any)
        else abort("Cannot determine zero for " + tp)
    }
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

  // wrap the given expression in a SoftReference so it can be gc-ed
  def mkSoftRef(expr: Tree): Tree = New(TypeTree(SoftReferenceClass.tpe), List(List(expr)))

  def mkCached(cvar: Symbol, expr: Tree): Tree = {
    val cvarRef = mkUnattributedRef(cvar)
    Block(
      List(
        If(Apply(Select(cvarRef, nme.eq), List(Literal(Constant(null)))),
           Assign(cvarRef, expr),
           EmptyTree)),
      cvarRef
    )
  }

  // Builds a tree of the form "{ lhs = rhs ; lhs  }"
  def mkAssignAndReturn(lhs: Symbol, rhs: Tree): Tree = {
    val lhsRef = mkUnattributedRef(lhs)
    Block(Assign(lhsRef, rhs) :: Nil, lhsRef)
  }

  def mkModuleVarDef(accessor: Symbol) = {
    val mval = (
      accessor.owner.newVariable(accessor.pos.focus, nme.moduleVarName(accessor.name))
      setInfo accessor.tpe.finalResultType
      setFlag (MODULEVAR)
    )

    mval.addAnnotation(AnnotationInfo(VolatileAttr.tpe, Nil, Nil))
    if (mval.owner.isClass) {
      mval setFlag (PRIVATE | LOCAL | SYNTHETIC)
      mval.owner.info.decls.enter(mval)
    }
    ValDef(mval)
  }

  // def m: T = { if (m$ eq null) m$ = new m$class(...) m$ }
  // where (...) are eventual outer accessors
  def mkCachedModuleAccessDef(accessor: Symbol, mvar: Symbol) =
    DefDef(accessor, mkCached(mvar, newModule(accessor, mvar.tpe)))

  def mkModuleAccessDef(accessor: Symbol, msym: Symbol) =
    DefDef(accessor, Select(This(msym.owner), msym))

  def newModule(accessor: Symbol, tpe: Type) =
    New(TypeTree(tpe),
        List(for (pt <- tpe.typeSymbol.primaryConstructor.info.paramTypes)
             yield This(accessor.owner.enclClass)))

  // def m: T;
  def mkModuleAccessDcl(accessor: Symbol) =
    DefDef(accessor setFlag lateDEFERRED, EmptyTree)

  def mkRuntimeCall(meth: Name, args: List[Tree]): Tree =
    Apply(Select(mkAttributedRef(ScalaRunTimeModule), meth), args)

  def mkRuntimeCall(meth: Name, targs: List[Type], args: List[Tree]): Tree =
    Apply(TypeApply(Select(mkAttributedRef(ScalaRunTimeModule), meth), targs map TypeTree), args)

  /** Make a synchronized block on 'monitor'. */
  def mkSynchronized(monitor: Tree, body: Tree): Tree =
    Apply(Select(monitor, Object_synchronized), List(body))

  def wildcardStar(tree: Tree) =
    atPos(tree.pos) { Typed(tree, Ident(tpnme.WILDCARD_STAR)) }

  def paramToArg(vparam: Symbol) = {
    val arg = Ident(vparam)
    if (isRepeatedParamType(vparam.tpe)) wildcardStar(arg)
    else arg
  }

  def paramToArg(vparam: ValDef) = {
    val arg = Ident(vparam.name)
    if (treeInfo.isRepeatedParamType(vparam.tpt)) wildcardStar(arg)
    else arg
  }

  /** Make forwarder to method `target', passing all parameters in `params' */
  def mkForwarder(target: Tree, vparamss: List[List[Symbol]]) =
    (target /: vparamss)((fn, vparams) => Apply(fn, vparams map paramToArg))

  /** Applies a wrapArray call to an array, making it a WrappedArray.
   *  Don't let a reference type parameter be inferred, in case it's a singleton:
   *  apply the element type directly.
   */
  def mkWrapArray(tree: Tree, elemtp: Type) = {
    val sym = elemtp.typeSymbol
    val meth: Name =
      if (isValueClass(sym)) "wrap"+sym.name+"Array"
      else if ((elemtp <:< AnyRefClass.tpe) && !isPhantomClass(sym)) "wrapRefArray"
      else "genericWrapArray"

    if (isValueClass(sym))
      Apply(Select(mkAttributedRef(PredefModule), meth), List(tree))
    else
      Apply(TypeApply(Select(mkAttributedRef(PredefModule), meth), List(TypeTree(elemtp))), List(tree))
  }

  /** Generate a cast for tree Tree representing Array with
   *  elem type elemtp to expected type pt.
   */
  def mkCastArray(tree: Tree, elemtp: Type, pt: Type) =
    if (elemtp.typeSymbol == AnyClass && isValueClass(tree.tpe.typeArgs.head.typeSymbol))
      mkCast(mkRuntimeCall("toObjectArray", List(tree)), pt)
    else
      mkCast(tree, pt)

  /** Translate names in Select/Ident nodes to type names.
   */
  def convertToTypeName(tree: Tree): Option[RefTree] = tree match {
    case Select(qual, name) => Some(Select(qual, name.toTypeName))
    case Ident(name)        => Some(Ident(name.toTypeName))
    case _                  => None
  }

  /** Try to convert Select(qual, name) to a SelectFromTypeTree.
   */
  def convertToSelectFromType(qual: Tree, origName: Name) = convertToTypeName(qual) match {
    case Some(qual1)  => SelectFromTypeTree(qual1 setPos qual.pos, origName.toTypeName)
    case _            => EmptyTree
  }

  /** Used in situations where you need to access value of an expression several times
   */
  def evalOnce(expr: Tree, owner: Symbol, unit: CompilationUnit)(within: (() => Tree) => Tree): Tree = {
    var used = false
    if (treeInfo.isPureExpr(expr)) {
      within(() => if (used) expr.duplicate else { used = true; expr })
    } else {
      val temp = owner.newValue(expr.pos.makeTransparent, unit.freshTermName("ev$"))
        .setFlag(SYNTHETIC).setInfo(expr.tpe)
      val containing = within(() => Ident(temp) setPos temp.pos.focus setType expr.tpe)
      ensureNonOverlapping(containing, List(expr))
      Block(List(ValDef(temp, expr)), containing) setPos (containing.pos union expr.pos)
    }
  }

  def evalOnceAll(exprs: List[Tree], owner: Symbol, unit: CompilationUnit)(within: (List[() => Tree]) => Tree): Tree = {
    val vdefs = new ListBuffer[ValDef]
    val exprs1 = new ListBuffer[() => Tree]
    val used = new Array[Boolean](exprs.length)
    var i = 0
    for (expr <- exprs) {
      if (treeInfo.isPureExpr(expr)) {
        exprs1 += {
          val idx = i
          () => if (used(idx)) expr.duplicate else { used(idx) = true; expr }
        }
      } else {
        val temp = owner.newValue(expr.pos.makeTransparent, unit.freshTermName("ev$"))
          .setFlag(SYNTHETIC).setInfo(expr.tpe)
        vdefs += ValDef(temp, expr)
        exprs1 += (() => Ident(temp) setPos temp.pos.focus setType expr.tpe)
      }
      i += 1
    }
    val prefix = vdefs.toList
    val containing = within(exprs1.toList)
    ensureNonOverlapping(containing, exprs)
    if (prefix.isEmpty) containing
    else Block(prefix, containing) setPos (prefix.head.pos union containing.pos)
  }

  /** Return a double-checked locking idiom around the syncBody tree. It guards with 'cond' and
   *  synchronizez on 'clazz.this'. Additional statements can be included after initialization,
   *  (outside the synchronized block).
   *
   *  The idiom works only if the condition is using a volatile field.
   *  @see http://www.cs.umd.edu/~pugh/java/memoryModel/DoubleCheckedLocking.html
   */
  def mkDoubleCheckedLocking(clazz: Symbol, cond: Tree, syncBody: List[Tree], stats: List[Tree]): Tree =
    mkDoubleCheckedLocking(mkAttributedThis(clazz), cond, syncBody, stats)

  def mkDoubleCheckedLocking(attrThis: Tree, cond: Tree, syncBody: List[Tree], stats: List[Tree]): Tree = {
    If(cond,
       Block(
         mkSynchronized(
           attrThis,
           If(cond, Block(syncBody: _*), EmptyTree)) ::
         stats: _*),
       EmptyTree)
  }
}
