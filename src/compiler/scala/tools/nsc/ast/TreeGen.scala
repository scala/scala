/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import scala.collection.mutable.ListBuffer
import symtab.Flags._
import scala.language.postfixOps

/** XXX to resolve: TreeGen only assumes global is a SymbolTable, but
 *  TreeDSL at the moment expects a Global.  Can we get by with SymbolTable?
 */
abstract class TreeGen extends scala.reflect.internal.TreeGen with TreeDSL {
  val global: Global

  import global._
  import definitions._

  /** Builds a fully attributed, synthetic wildcard import node.
   */
  def mkWildcardImport(pkg: Symbol): Import =
    mkImportFromSelector(pkg, ImportSelector.wildList)

  /** Builds a fully attributed, synthetic import node.
    * import `qualSym`.{`name` => `toName`}
    */
  def mkImport(qualSym: Symbol, name: Name, toName: Name): Import =
    mkImportFromSelector(qualSym, ImportSelector(name, 0, toName, 0) :: Nil)

  private def mkImportFromSelector(qualSym: Symbol, selector: List[ImportSelector]): Import = {
    assert(qualSym ne null, this)
    val qual = gen.mkAttributedStableRef(qualSym)
    val importSym = (
      NoSymbol
        newImport NoPosition
          setFlag SYNTHETIC
          setInfo ImportType(qual)
    )
    val importTree = (
      Import(qual, selector)
        setSymbol importSym
          setType NoType
    )
    importTree
  }

  // wrap the given expression in a SoftReference so it can be gc-ed
  def mkSoftRef(expr: Tree): Tree = atPos(expr.pos) {
    val constructor = SoftReferenceClass.info.nonPrivateMember(nme.CONSTRUCTOR).suchThat(_.paramss.flatten.size == 1)
    NewFromConstructor(constructor, expr)
  }

  // Builds a tree of the form "{ lhs = rhs ; lhs  }"
  def mkAssignAndReturn(lhs: Symbol, rhs: Tree): Tree = {
    def lhsRef = if (lhs.owner.isClass) Select(This(lhs.owner), lhs) else Ident(lhs)
    Block(Assign(lhsRef, rhs) :: Nil, lhsRef)
  }

  def newModule(accessor: Symbol, tpe: Type) = {
    val ps = tpe.typeSymbol.primaryConstructor.info.paramTypes
    if (ps.isEmpty) New(tpe)
    else New(tpe, This(accessor.owner.enclClass))
  }

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
  override def mkCast(tree: Tree, pt: Type): Tree = {
    debuglog("casting " + tree + ":" + tree.tpe + " to " + pt + " at phase: " + phase)
    assert(!tree.tpe.isInstanceOf[MethodType], tree)
    assert(pt eq pt.normalize, tree +" : "+ debugString(pt) +" ~>"+ debugString(pt.normalize))
    atPos(tree.pos) {
      mkAsInstanceOf(tree, pt, any = !phase.next.erasedTypes, wrapInApply = isAtPhaseAfter(currentRun.uncurryPhase))
    }
  }

  // drop annotations generated by CPS plugin etc, since its annotationchecker rejects T @cps[U] <: Any
  // let's assume for now annotations don't affect casts, drop them there, and bring them back using the outer Typed tree
  def mkCastPreservingAnnotations(tree: Tree, pt: Type) =
    Typed(mkCast(tree, pt.withoutAnnotations.dealias), TypeTree(pt))

  /** Generate a cast for tree Tree representing Array with
   *  elem type elemtp to expected type pt.
   */
  def mkCastArray(tree: Tree, elemtp: Type, pt: Type) =
    if (elemtp.typeSymbol == AnyClass && isPrimitiveValueType(tree.tpe.typeArgs.head))
      mkCast(mkRuntimeCall(nme.toObjectArray, List(tree)), pt)
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

  /** Create a ValDef initialized to the given expression, setting the
   *  symbol to its packed type, and an function for creating Idents
   *  which refer to it.
   */
  private def mkPackedValDef(expr: Tree, owner: Symbol, name: Name): (ValDef, () => Ident) = {
    val packedType = typer.packedType(expr, owner)
    val sym = owner.newValue(name.toTermName, expr.pos.makeTransparent, SYNTHETIC) setInfo packedType

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
   *  synchronizes on `clazz.this`. Additional statements can be included after initialization,
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

  /** Creates a tree representing new Object { stats }.
   *  To make sure an anonymous subclass of Object is created,
   *  if there are no stats, a () is added.
   */
  def mkAnonymousNew(stats: List[Tree]): Tree = {
    val stats1 = if (stats.isEmpty) List(Literal(Constant(()))) else stats
    mkNew(Nil, noSelfType, stats1, NoPosition, NoPosition)
  }

  /**
   * Create a method based on a Function
   *
   * Used both to under `-Ydelambdafy:method` create a lifted function and
   * under `-Ydelambdafy:inline` to create the apply method on the anonymous
   * class.
   *
   * It creates a method definition with value params cloned from the
   * original lambda. Then it calls a supplied function to create
   * the body and types the result. Finally
   * everything is wrapped up in a DefDef
   *
   * @param owner The owner for the new method
   * @param name name for the new method
   * @param additionalFlags flags to be put on the method in addition to FINAL
   */
  def mkMethodFromFunction(localTyper: analyzer.Typer)
                          (fun: Function, owner: Symbol, name: TermName, additionalFlags: FlagSet = NoFlags) = {
    val funParams = fun.vparams map (_.symbol)
    val formals :+ restpe = fun.tpe.typeArgs

    val methSym = owner.newMethod(name, fun.pos, FINAL | additionalFlags)

    val paramSyms = map2(formals, fun.vparams) {
      (tp, vparam) => methSym.newSyntheticValueParam(tp, vparam.name)
    }

    methSym setInfo MethodType(paramSyms, restpe.deconst)

    fun.body.substituteSymbols(funParams, paramSyms)
    fun.body changeOwner (fun.symbol -> methSym)

    val methDef = DefDef(methSym, fun.body)

    // Have to repack the type to avoid mismatches when existentials
    // appear in the result - see SI-4869.
    methDef.tpt setType localTyper.packedType(fun.body, methSym).deconst
    methDef
  }
}
