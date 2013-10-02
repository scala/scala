package scala
package reflect
package internal

import Flags._
import util._

abstract class TreeGen extends macros.TreeBuilder {
  val global: SymbolTable

  import global._
  import definitions._

  def rootId(name: Name)             = Select(Ident(nme.ROOTPKG), name)
  def rootScalaDot(name: Name)       = Select(rootId(nme.scala_) setSymbol ScalaPackage, name)
  def scalaDot(name: Name)           = Select(Ident(nme.scala_) setSymbol ScalaPackage, name)
  def scalaAnnotationDot(name: Name) = Select(scalaDot(nme.annotation), name)
  def scalaAnyRefConstrRaw           = scalaDot(tpnme.AnyRef)
  def scalaAnyRefConstr              = scalaAnyRefConstrRaw setSymbol AnyRefClass // used in ide

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

  def mkNullaryCall(method: Symbol, targs: List[Type]): Tree =
    mkTypeApply(mkAttributedRef(method), targs map TypeTree)

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
  def mkAttributedRef(pre: Type, sym: Symbol): RefTree = {
    val qual = mkAttributedQualifier(pre)
    qual match {
      case EmptyTree                                  => mkAttributedIdent(sym)
      case This(clazz) if qual.symbol.isEffectiveRoot => mkAttributedIdent(sym)
      case _                                          => mkAttributedSelect(qual, sym)
    }
  }

  /** Builds a reference to given symbol. */
  def mkAttributedRef(sym: Symbol): RefTree =
    if (sym.owner.isClass) mkAttributedRef(sym.owner.thisType, sym)
    else mkAttributedIdent(sym)

  def mkUnattributedRef(sym: Symbol): RefTree = mkUnattributedRef(sym.fullNameAsName('.'))

  def mkUnattributedRef(fullName: Name): RefTree = {
    val hd :: tl = nme.segments(fullName.toString, assumeTerm = fullName.isTermName)
    tl.foldLeft(Ident(hd): RefTree)(Select(_,_))
  }

  /** Replaces tree type with a stable type if possible */
  def stabilize(tree: Tree): Tree = stableTypeFor(tree) match {
    case NoType => tree
    case tp     => tree setType tp
  }

  /** Computes stable type for a tree if possible */
  def stableTypeFor(tree: Tree): Type = (
    if (!treeInfo.admitsTypeSelection(tree)) NoType
    else tree match {
      case This(_)         => ThisType(tree.symbol)
      case Ident(_)        => singleType(tree.symbol.owner.thisType, tree.symbol)
      case Select(qual, _) => singleType(qual.tpe, tree.symbol)
      case _               => NoType
    }
  )

  /** Builds a reference with stable type to given symbol */
  def mkAttributedStableRef(pre: Type, sym: Symbol): Tree =
    stabilize(mkAttributedRef(pre, sym))

  def mkAttributedStableRef(sym: Symbol): Tree =
    stabilize(mkAttributedRef(sym))

  def mkAttributedThis(sym: Symbol): This =
    This(sym.name.toTypeName) setSymbol sym setType sym.thisType

  def mkAttributedIdent(sym: Symbol): RefTree =
    Ident(sym.name) setSymbol sym setType sym.tpeHK

  def mkAttributedSelect(qual: Tree, sym: Symbol): RefTree = {
    // Tests involving the repl fail without the .isEmptyPackage condition.
    if (qual.symbol != null && (qual.symbol.isEffectiveRoot || qual.symbol.isEmptyPackage))
      mkAttributedIdent(sym)
    else {
      // Have to recognize anytime a selection is made on a package
      // so it can be rewritten to foo.bar.`package`.name rather than
      // foo.bar.name if name is in the package object.
      // TODO - factor out the common logic between this and
      // the Typers method "isInPackageObject", used in typedIdent.
      val qualsym = (
        if (qual.tpe ne null) qual.tpe.typeSymbol
        else if (qual.symbol ne null) qual.symbol
        else NoSymbol
      )
      val needsPackageQualifier = (
           (sym ne null)
        && qualsym.isPackage
        && !(sym.isDefinedInPackage || sym.moduleClass.isDefinedInPackage) // SI-7817 work around strangeness in post-flatten `Symbol#owner`
      )
      val pkgQualifier =
        if (needsPackageQualifier) {
          // The owner of a symbol which requires package qualification may be the
          // package object iself, but it also could be any superclass of the package
          // object.  In the latter case, we must go through the qualifier's info
          // to obtain the right symbol.
          val packageObject = if (sym.owner.isModuleClass) sym.owner.sourceModule else qual.tpe member nme.PACKAGE
          Select(qual, nme.PACKAGE) setSymbol packageObject setType singleType(qual.tpe, packageObject)
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
  def mkAppliedTypeTree(fun: Tree, targs: List[Tree]): Tree =
    if (targs.isEmpty) fun else AppliedTypeTree(fun, targs)
  def mkAttributedTypeApply(target: Tree, method: Symbol, targs: List[Type]): Tree =
    mkTypeApply(mkAttributedSelect(target, method), targs map TypeTree)

  private def mkSingleTypeApply(value: Tree, tpe: Type, what: Symbol, wrapInApply: Boolean) = {
    val tapp = mkAttributedTypeApply(value, what, tpe.dealias :: Nil)
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
    if ((pt == UnitTpe) || (tpe <:< pt)) tree
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
  def mkNil: Tree = mkAttributedRef(NilModule)

  /** Builds a tree representing an undefined local, as in
   *    var x: T = _
   *  which is appropriate to the given Type.
   */
  def mkZero(tp: Type): Tree = tp.typeSymbol match {
    case NothingClass => mkMethodCall(Predef_???, Nil) setType NothingTpe
    case _            => Literal(mkConstantZero(tp)) setType tp
  }

  def mkConstantZero(tp: Type): Constant = tp.typeSymbol match {
    case UnitClass    => Constant(())
    case BooleanClass => Constant(false)
    case FloatClass   => Constant(0.0f)
    case DoubleClass  => Constant(0.0d)
    case ByteClass    => Constant(0.toByte)
    case ShortClass   => Constant(0.toShort)
    case IntClass     => Constant(0)
    case LongClass    => Constant(0L)
    case CharClass    => Constant(0.toChar)
    case _            => Constant(null)
  }

  /** Wrap an expression in a named argument. */
  def mkNamedArg(name: Name, tree: Tree): Tree = mkNamedArg(Ident(name), tree)
  def mkNamedArg(lhs: Tree, rhs: Tree): Tree = atPos(rhs.pos)(AssignOrNamedArg(lhs, rhs))

  /** Builds a tuple */
  def mkTuple(elems: List[Tree]): Tree =
    if (elems.isEmpty) Literal(Constant(()))
    else Apply(
      Select(mkAttributedRef(TupleClass(elems.length).caseModule), nme.apply),
      elems)

  // tree1 AND tree2
  def mkAnd(tree1: Tree, tree2: Tree): Tree =
    Apply(Select(tree1, Boolean_and), List(tree2))

  // tree1 OR tree2
  def mkOr(tree1: Tree, tree2: Tree): Tree =
    Apply(Select(tree1, Boolean_or), List(tree2))

  def mkRuntimeUniverseRef: Tree = {
    assert(ReflectRuntimeUniverse != NoSymbol)
    mkAttributedRef(ReflectRuntimeUniverse) setType singleType(ReflectRuntimeUniverse.owner.thisPrefix, ReflectRuntimeUniverse)
  }

  def mkSeqApply(arg: Tree): Apply = {
    val factory = Select(gen.mkAttributedRef(SeqModule), nme.apply)
    Apply(factory, List(arg))
  }

  def mkSuperInitCall: Select = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)

  /** Generates a template with constructor corresponding to
   *
   *  constrmods (vparams1_) ... (vparams_n) preSuper { presupers }
   *  extends superclass(args_1) ... (args_n) with mixins { self => body }
   *
   *  This gets translated to
   *
   *  extends superclass with mixins { self =>
   *    presupers' // presupers without rhs
   *    vparamss   // abstract fields corresponding to value parameters
   *    def <init>(vparamss) {
   *      presupers
   *      super.<init>(args)
   *    }
   *    body
   *  }
   */
  def mkTemplate(parents: List[Tree], self: ValDef, constrMods: Modifiers,
                 vparamss: List[List[ValDef]], body: List[Tree], superPos: Position = NoPosition): Template = {
    /* Add constructor to template */

    // create parameters for <init> as synthetic trees.
    var vparamss1 = mmap(vparamss) { vd =>
      atPos(vd.pos.focus) {
        val mods = Modifiers(vd.mods.flags & (IMPLICIT | DEFAULTPARAM | BYNAMEPARAM) | PARAM | PARAMACCESSOR)
        ValDef(mods withAnnotations vd.mods.annotations, vd.name, vd.tpt.duplicate, vd.rhs.duplicate)
      }
    }
    val (edefs, rest) = body span treeInfo.isEarlyDef
    val (evdefs, etdefs) = edefs partition treeInfo.isEarlyValDef
    val gvdefs = evdefs map {
      case vdef @ ValDef(_, _, tpt, _) =>
        copyValDef(vdef)(
        // atPos for the new tpt is necessary, since the original tpt might have no position
        // (when missing type annotation for ValDef for example), so even though setOriginal modifies the
        // position of TypeTree, it would still be NoPosition. That's what the author meant.
        tpt = atPos(vdef.pos.focus)(TypeTree() setOriginal tpt setPos tpt.pos.focus),
        rhs = EmptyTree
      )
    }
    val lvdefs = evdefs collect { case vdef: ValDef => copyValDef(vdef)(mods = vdef.mods | PRESUPER) }

    val constr = {
      if (constrMods.isTrait) {
        if (body forall treeInfo.isInterfaceMember) None
        else Some(
          atPos(wrappingPos(superPos, lvdefs)) (
            DefDef(NoMods, nme.MIXIN_CONSTRUCTOR, Nil, ListOfNil, TypeTree(), Block(lvdefs, Literal(Constant())))))
      }
      else {
        // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
        if (vparamss1.isEmpty || !vparamss1.head.isEmpty && vparamss1.head.head.mods.isImplicit)
          vparamss1 = List() :: vparamss1
        val superCall = pendingSuperCall // we can't know in advance which of the parents will end up as a superclass
                                         // this requires knowing which of the parents is a type macro and which is not
                                         // and that's something that cannot be found out before typer
                                         // (the type macros aren't in the trunk yet, but there is a plan for them to land there soon)
                                         // this means that we don't know what will be the arguments of the super call
                                         // therefore here we emit a dummy which gets populated when the template is named and typechecked
        Some(
          // TODO: previously this was `wrappingPos(superPos, lvdefs ::: argss.flatten)`
          // is it going to be a problem that we can no longer include the `argss`?
          atPos(wrappingPos(superPos, lvdefs)) (
            DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), Block(lvdefs ::: List(superCall), Literal(Constant())))))
      }
    }
    constr foreach (ensureNonOverlapping(_, parents ::: gvdefs, focus = false))
    // Field definitions for the class - remove defaults.
    val fieldDefs = vparamss.flatten map (vd => copyValDef(vd)(mods = vd.mods &~ DEFAULTPARAM, rhs = EmptyTree))

    global.Template(parents, self, gvdefs ::: fieldDefs ::: constr ++: etdefs ::: rest)
  }

  def mkParents(ownerMods: Modifiers, parents: List[Tree], parentPos: Position = NoPosition) =
    if (ownerMods.isCase) parents ::: List(scalaDot(tpnme.Product), scalaDot(tpnme.Serializable))
    else if (parents.isEmpty) atPos(parentPos)(scalaAnyRefConstrRaw) :: Nil
    else parents

  def mkClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], templ: Template): ClassDef = {
    val isInterface = mods.isTrait && (templ.body forall treeInfo.isInterfaceMember)
    val mods1 = if (isInterface) (mods | Flags.INTERFACE) else mods
    ClassDef(mods1, name, tparams, templ)
  }

  /** Create positioned tree representing an object creation <new parents { stats }
   *  @param npos  the position of the new
   *  @param cpos  the position of the anonymous class starting with parents
   */
  def mkNew(parents: List[Tree], self: ValDef, stats: List[Tree],
            npos: Position, cpos: Position): Tree =
    if (parents.isEmpty)
      mkNew(List(scalaAnyRefConstr), self, stats, npos, cpos)
    else if (parents.tail.isEmpty && stats.isEmpty) {
      // `Parsers.template` no longer differentiates tpts and their argss
      // e.g. `C()` will be represented as a single tree Apply(Ident(C), Nil)
      // instead of parents = Ident(C), argss = Nil as before
      // this change works great for things that are actually templates
      // but in this degenerate case we need to perform postprocessing
      val app = treeInfo.dissectApplied(parents.head)
      atPos(npos union cpos) { New(app.callee, app.argss) }
    } else {
      val x = tpnme.ANON_CLASS_NAME
      atPos(npos union cpos) {
        Block(
          List(
            atPos(cpos) {
              ClassDef(
                Modifiers(FINAL), x, Nil,
                mkTemplate(parents, self, NoMods, ListOfNil, stats, cpos.focus))
            }),
          atPos(npos) {
            New(
              Ident(x) setPos npos.focus,
              Nil)
          }
        )
      }
    }

  /** Create a tree representing the function type (argtpes) => restpe */
  def mkFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(newTypeName("Function" + argtpes.length)), argtpes ::: List(restpe))

  /** Create block of statements `stats`  */
  def mkBlock(stats: List[Tree]): Tree =
    if (stats.isEmpty) Literal(Constant(()))
    else if (!stats.last.isTerm) Block(stats, Literal(Constant(())))
    else if (stats.length == 1) stats.head
    else Block(stats.init, stats.last)

  def mkTreeOrBlock(stats: List[Tree]) = stats match {
    case Nil => EmptyTree
    case head :: Nil => head
    case _ => gen.mkBlock(stats)
  }

  /** Create a tree representing an assignment <lhs = rhs> */
  def mkAssign(lhs: Tree, rhs: Tree): Tree = lhs match {
    case Apply(fn, args) =>
      Apply(atPos(fn.pos)(Select(fn, nme.update)), args :+ rhs)
    case _ =>
      Assign(lhs, rhs)
  }
}
