/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.collection.mutable.ListBuffer
import symtab.Flags._
import symtab.SymbolTable
import language.postfixOps

/** XXX to resolve: TreeGen only assumes global is a SymbolTable, but
 *  TreeDSL at the moment expects a Global.  Can we get by with SymbolTable?
 */
abstract class TreeGen extends reflect.internal.TreeGen with TreeDSL {
  val global: Global

  import global._
  import definitions._

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

  /** Builds a fully attributed wildcard import node.
   */
  def mkWildcardImport(pkg: Symbol): Import = {
    assert(pkg ne null, this)
    val qual = gen.mkAttributedStableRef(pkg)
    val importSym = (
      NoSymbol
        newImport NoPosition
          setFlag SYNTHETIC
          setInfo analyzer.ImportType(qual)
    )
    val importTree = (
      Import(qual, List(ImportSelector(nme.WILDCARD, -1, null, -1)))
        setSymbol importSym
          setType NoType
    )
    importTree
  }

  // wrap the given expression in a SoftReference so it can be gc-ed
  def mkSoftRef(expr: Tree): Tree = atPos(expr.pos)(New(SoftReferenceClass.tpe, expr))

  // annotate the expression with @unchecked
  def mkUnchecked(expr: Tree): Tree = atPos(expr.pos) {
    // This can't be "Annotated(New(UncheckedClass), expr)" because annotations
    // are very picky about things and it crashes the compiler with "unexpected new".
    Annotated(New(scalaDot(UncheckedClass.name), List(Nil)), expr)
  }
  // if it's a Match, mark the selector unchecked; otherwise nothing.
  def mkUncheckedMatch(tree: Tree) = tree match {
    case Match(selector, cases) => atPos(tree.pos)(Match(mkUnchecked(selector), cases))
    case _                      => tree
  }

  def mkSynthSwitchSelector(expr: Tree): Tree = atPos(expr.pos) {
    // This can't be "Annotated(New(SwitchClass), expr)" because annotations
    // are very picky about things and it crashes the compiler with "unexpected new".
    Annotated(Ident(nme.synthSwitch), expr)
  }

  def hasSynthCaseSymbol(t: Tree) = (t.symbol ne null) && (t.symbol hasFlag (CASE | SYNTHETIC))

  // TODO: would be so much nicer if we would know during match-translation (i.e., type checking)
  // whether we should emit missingCase-style apply (and isDefinedAt), instead of transforming trees post-factum
  class MatchMatcher {
    def caseMatch(orig: Tree, selector: Tree, cases: List[CaseDef], wrap: Tree => Tree): Tree = unknownTree(orig)
    def caseVirtualizedMatch(orig: Tree, _match: Tree, targs: List[Tree], scrut: Tree, matcher: Tree): Tree = unknownTree(orig)
    def caseVirtualizedMatchOpt(orig: Tree, prologue: List[Tree], cases: List[Tree], matchEndDef: Tree, wrap: Tree => Tree): Tree = unknownTree(orig)

    def genVirtualizedMatch(prologue: List[Tree], cases: List[Tree], matchEndDef: Tree): Tree = Block(prologue ++ cases, matchEndDef)

    def apply(matchExpr: Tree): Tree = matchExpr match {
      // old-style match or virtpatmat switch
      case Match(selector, cases) => // println("simple match: "+ (selector, cases) + "for:\n"+ matchExpr )
        caseMatch(matchExpr, selector, cases, identity)
      // old-style match or virtpatmat switch
      case Block((vd: ValDef) :: Nil, orig@Match(selector, cases)) => // println("block match: "+ (selector, cases, vd) + "for:\n"+ matchExpr )
        caseMatch(matchExpr, selector, cases, m => copyBlock(matchExpr, List(vd), m))
      // virtpatmat
      case Apply(Apply(TypeApply(Select(tgt, nme.runOrElse), targs), List(scrut)), List(matcher)) if opt.virtPatmat => // println("virt match: "+ (tgt, targs, scrut, matcher) + "for:\n"+ matchExpr )
        caseVirtualizedMatch(matchExpr, tgt, targs, scrut, matcher)
      // optimized version of virtpatmat
      case Block(stats, matchEndDef) if opt.virtPatmat && (stats forall hasSynthCaseSymbol) =>
        // the assumption is once we encounter a case, the remainder of the block will consist of cases
        // the prologue may be empty, usually it is the valdef that stores the scrut
        val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
        caseVirtualizedMatchOpt(matchExpr, prologue, cases, matchEndDef, identity)
      // optimized version of virtpatmat
      case Block(outerStats, orig@Block(stats, matchEndDef)) if opt.virtPatmat && (stats forall hasSynthCaseSymbol) =>
        val (prologue, cases) = stats span (s => !s.isInstanceOf[LabelDef])
        caseVirtualizedMatchOpt(matchExpr, prologue, cases, matchEndDef, m => copyBlock(matchExpr, outerStats, m))
      case other =>
        unknownTree(other)
    }

    def unknownTree(t: Tree): Tree = throw new MatchError(t)
    def copyBlock(orig: Tree, stats: List[Tree], expr: Tree): Block = Block(stats, expr)

    def dropSyntheticCatchAll(cases: List[CaseDef]): List[CaseDef] =
      if (!opt.virtPatmat) cases
      else cases filter {
             case CaseDef(pat, EmptyTree, Throw(Apply(Select(New(exTpt), nme.CONSTRUCTOR), _))) if (treeInfo.isWildcardArg(pat) && (exTpt.tpe.typeSymbol eq MatchErrorClass)) => false
             case CaseDef(pat, guard, body) => true
           }
  }

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
    val inClass    = accessor.owner.isClass
    val extraFlags = if (inClass) PrivateLocal | SYNTHETIC else 0

    val mval = (
      accessor.owner.newVariable(nme.moduleVarName(accessor.name), accessor.pos.focus, MODULEVAR | extraFlags)
        setInfo accessor.tpe.finalResultType
        addAnnotation VolatileAttr
    )
    if (inClass)
      mval.owner.info.decls enter mval

    ValDef(mval)
  }

  // def m: T = { if (m$ eq null) m$ = new m$class(...) m$ }
  // where (...) are eventual outer accessors
  def mkCachedModuleAccessDef(accessor: Symbol, mvar: Symbol) =
    DefDef(accessor, mkCached(mvar, newModule(accessor, mvar.tpe)))

  def mkModuleAccessDef(accessor: Symbol, msym: Symbol) =
    DefDef(accessor, Select(This(msym.owner), msym))

  def newModule(accessor: Symbol, tpe: Type) = {
    val ps = tpe.typeSymbol.primaryConstructor.info.paramTypes
    if (ps.isEmpty) New(tpe)
    else New(tpe, This(accessor.owner.enclClass))
  }

  // def m: T;
  def mkModuleAccessDcl(accessor: Symbol) =
    DefDef(accessor setFlag lateDEFERRED, EmptyTree)

  def mkRuntimeCall(meth: Name, args: List[Tree]): Tree =
    mkRuntimeCall(meth, Nil, args)

  def mkRuntimeCall(meth: Name, targs: List[Type], args: List[Tree]): Tree =
    mkMethodCall(ScalaRunTimeModule, meth, targs, args)

  def mkSysErrorCall(message: String): Tree =
    mkMethodCall(Sys_error, List(Literal(Constant(message))))

  /** A creator for a call to a scala.reflect.Manifest or ClassManifest factory method.
   *
   *  @param    full          full or partial manifest (target will be Manifest or ClassManifest)
   *  @param    constructor   name of the factory method (e.g. "classType")
   *  @param    tparg         the type argument
   *  @param    args          value arguments
   *  @return   the tree
   */
  def mkManifestFactoryCall(full: Boolean, constructor: String, tparg: Type, args: List[Tree]): Tree =
    mkMethodCall(
      if (full) FullManifestModule else PartialManifestModule,
      newTermName(constructor),
      List(tparg),
      args
    )

  /** Make a synchronized block on 'monitor'. */
  def mkSynchronized(monitor: Tree, body: Tree): Tree =
    Apply(Select(monitor, Object_synchronized), List(body))

  def mkAppliedTypeForCase(clazz: Symbol): Tree = {
    val numParams = clazz.typeParams.size
    if (clazz.typeParams.isEmpty) Ident(clazz)
    else AppliedTypeTree(Ident(clazz), 1 to numParams map (_ => Bind(tpnme.WILDCARD, EmptyTree)) toList)
  }
  def mkBindForCase(patVar: Symbol, clazz: Symbol, targs: List[Type]): Tree = {
    Bind(patVar, Typed(Ident(nme.WILDCARD),
      if (targs.isEmpty) mkAppliedTypeForCase(clazz)
      else AppliedTypeTree(Ident(clazz), targs map TypeTree)
    ))
  }
  def mkSuperSelect = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)

  def wildcardStar(tree: Tree) =
    atPos(tree.pos) { Typed(tree, Ident(tpnme.WILDCARD_STAR)) }

  def paramToArg(vparam: Symbol): Tree =
    paramToArg(Ident(vparam), isRepeatedParamType(vparam.tpe))

  def paramToArg(vparam: ValDef): Tree =
    paramToArg(Ident(vparam.name), treeInfo.isRepeatedParamType(vparam.tpt))

  def paramToArg(arg: Ident, isRepeatedParam: Boolean): Tree  =
    if (isRepeatedParam) wildcardStar(arg) else arg

  /** Make forwarder to method `target`, passing all parameters in `params` */
  def mkForwarder(target: Tree, vparamss: List[List[Symbol]]) =
    (target /: vparamss)((fn, vparams) => Apply(fn, vparams map paramToArg))

  /** Applies a wrapArray call to an array, making it a WrappedArray.
   *  Don't let a reference type parameter be inferred, in case it's a singleton:
   *  apply the element type directly.
   */
  def mkWrapArray(tree: Tree, elemtp: Type) = {
    mkMethodCall(
      PredefModule,
      wrapArrayMethodName(elemtp),
      if (isPrimitiveValueType(elemtp)) Nil else List(elemtp),
      List(tree)
    )
  }

  /** Cast `tree` to type `pt` by creating
   *  one of the calls of the form
   *
   *    x.asInstanceOf[`pt`]     up to phase uncurry
   *    x.asInstanceOf[`pt`]()   if after uncurry but before erasure
   *    x.$asInstanceOf[`pt`]()  if at or after erasure
   */
  def mkCast(tree: Tree, pt: Type): Tree = {
    debuglog("casting " + tree + ":" + tree.tpe + " to " + pt + " at phase: " + phase)
    assert(!tree.tpe.isInstanceOf[MethodType], tree)
    assert(pt eq pt.normalize, tree +" : "+ debugString(pt) +" ~>"+ debugString(pt.normalize))
    atPos(tree.pos) {
      mkAsInstanceOf(tree, pt, any = !phase.next.erasedTypes, wrapInApply = isAtPhaseAfter(currentRun.uncurryPhase))
    }
  }

  /** Generate a cast for tree Tree representing Array with
   *  elem type elemtp to expected type pt.
   */
  def mkCastArray(tree: Tree, elemtp: Type, pt: Type) =
    if (elemtp.typeSymbol == AnyClass && isPrimitiveValueType(tree.tpe.typeArgs.head))
      mkCast(mkRuntimeCall(nme.toObjectArray, List(tree)), pt)
    else
      mkCast(tree, pt)

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

  /** Create a ValDef initialized to the given expression, setting the
   *  symbol to its packed type, and an function for creating Idents
   *  which refer to it.
   */
  private def mkPackedValDef(expr: Tree, owner: Symbol, name: Name): (ValDef, () => Ident) = {
    val packedType = typer.packedType(expr, owner)
    val sym = owner.newValue(name, expr.pos.makeTransparent, SYNTHETIC) setInfo packedType

    (ValDef(sym, expr), () => Ident(sym) setPos sym.pos.focus setType expr.tpe)
  }

  /** Used in situations where you need to access value of an expression several times
   */
  def evalOnce(expr: Tree, owner: Symbol, unit: CompilationUnit)(within: (() => Tree) => Tree): Tree = {
    var used = false
    if (treeInfo.isExprSafeToInline(expr)) {
      within(() => if (used) expr.duplicate else { used = true; expr })
    }
    else {
      val (valDef, identFn) = mkPackedValDef(expr, owner, unit.freshTermName("ev$"))
      val containing = within(identFn)
      ensureNonOverlapping(containing, List(expr))
      Block(List(valDef), containing) setPos (containing.pos union expr.pos)
    }
  }

  def evalOnceAll(exprs: List[Tree], owner: Symbol, unit: CompilationUnit)(within: (List[() => Tree]) => Tree): Tree = {
    val vdefs = new ListBuffer[ValDef]
    val exprs1 = new ListBuffer[() => Tree]
    val used = new Array[Boolean](exprs.length)
    var i = 0
    for (expr <- exprs) {
      if (treeInfo.isExprSafeToInline(expr)) {
        exprs1 += {
          val idx = i
          () => if (used(idx)) expr.duplicate else { used(idx) = true; expr }
        }
      }
      else {
        val (valDef, identFn) = mkPackedValDef(expr, owner, unit.freshTermName("ev$"))
        vdefs += valDef
        exprs1 += identFn
      }
      i += 1
    }
    val prefix = vdefs.toList
    val containing = within(exprs1.toList)
    ensureNonOverlapping(containing, exprs)
    if (prefix.isEmpty) containing
    else Block(prefix, containing) setPos (prefix.head.pos union containing.pos)
  }

  /** Return the synchronized part of the double-checked locking idiom around the syncBody tree. It guards with `cond` and
   *  synchronizez on `clazz.this`. Additional statements can be included after initialization,
   *  (outside the synchronized block).
   *
   *  The idiom works only if the condition is using a volatile field.
   *  @see http://www.cs.umd.edu/~pugh/java/memoryModel/DoubleCheckedLocking.html
   */
  def mkSynchronizedCheck(clazz: Symbol, cond: Tree, syncBody: List[Tree], stats: List[Tree]): Tree =
    mkSynchronizedCheck(mkAttributedThis(clazz), cond, syncBody, stats)
    
  def mkSynchronizedCheck(attrThis: Tree, cond: Tree, syncBody: List[Tree], stats: List[Tree]): Tree = 
    Block(mkSynchronized(
      attrThis,
      If(cond, Block(syncBody: _*), EmptyTree)) ::
      stats: _*)
}
