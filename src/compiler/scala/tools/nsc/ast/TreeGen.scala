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
  def mkSynchronized(monitor: Tree)(body: Tree): Tree =
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
    assert(!pt.isInstanceOf[MethodType], tree)
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

  /** Creates a tree representing new Object { stats }.
   *  To make sure an anonymous subclass of Object is created,
   *  if there are no stats, a () is added.
   */
  def mkAnonymousNew(stats: List[Tree]): Tree = {
    val stats1 = if (stats.isEmpty) List(Literal(Constant(()))) else stats
    mkNew(Nil, noSelfType, stats1, NoPosition, NoPosition)
  }


  // Construct a method to implement `fun`'s single abstract method (`apply`, when `fun.tpe` is a built-in function type)
  def mkMethodFromFunction(localTyper: analyzer.Typer)(owner: Symbol, fun: Function) = {
    // TODO: treat FunctionN like any other SAM -- drop `&& !isFunctionType(fun.tpe)`
    val sam = if (!isFunctionType(fun.tpe)) samOf(fun.tpe) else NoSymbol
    if (!sam.exists) mkMethodForFunctionBody(localTyper)(owner, fun, nme.apply)()
    else {
      val samMethType = fun.tpe memberInfo sam
      mkMethodForFunctionBody(localTyper)(owner, fun, sam.name.toTermName)(methParamProtos = samMethType.params, resTp = samMethType.resultType)
    }
  }

  // used to create the lifted method that holds a function's body
  def mkLiftedFunctionBodyMethod(localTyper: global.analyzer.Typer)(owner: global.Symbol, fun: global.Function) = {
    def nonLocalEnclosingMember(sym: Symbol): Symbol = {
      if (sym.isLocalDummy) sym.enclClass.primaryConstructor
      else if (sym.isLocalToBlock) nonLocalEnclosingMember(sym.originalOwner)
      else sym
    }
    val ownerName = nonLocalEnclosingMember(fun.symbol.originalOwner).name match {
      case nme.CONSTRUCTOR => nme.NEWkw // do as javac does for the suffix, prefer "new" to "$lessinit$greater$1"
      case x => x.dropLocal
    }
    val newName = nme.ANON_FUN_NAME.append(nme.NAME_JOIN_STRING).append(ownerName)
    mkMethodForFunctionBody(localTyper)(owner, fun, newName)(additionalFlags = ARTIFACT)
  }


  // the result type of a function or corresponding SAM type
  private def functionResultType(tp: Type): Type = {
    val dealiased = tp.dealiasWiden
    if (isFunctionTypeDirect(dealiased)) dealiased.typeArgs.last
    else samOf(tp) match {
      case samSym if samSym.exists => tp.memberInfo(samSym).resultType.deconst
      case _ => NoType
    }
  }

  /**
    * Lift a Function's body to a method. For use during Uncurry, where Function nodes have type FunctionN[T1, ..., Tn, R]
    *
    * It creates a method definition with value params derived from the original lambda
    * or `methParamProtos` (used to create the correct override for sam methods).
    *
    * Replace the `fun.vparams` symbols by the newly created method params,
    * changes owner of `fun.body` from `fun.symbol` to resulting method's symbol.
    *
    * @param owner The owner for the new method
    * @param fun  the function to take the body from
    * @param name name for the new method
    * @param additionalFlags flags to be put on the method in addition to FINAL
    */
  private def mkMethodForFunctionBody(localTyper: analyzer.Typer)
                                     (owner: Symbol, fun: Function, name: TermName)
                                     (methParamProtos: List[Symbol] = fun.vparams.map(_.symbol),
                                      resTp: Type = functionResultType(fun.tpe),
                                      additionalFlags: FlagSet = NoFlags): DefDef = {
    val methSym = owner.newMethod(name, fun.pos, FINAL | additionalFlags)
    // for sams, methParamProtos is the parameter symbols for the sam's method, so that we generate the correct override (based on parmeter types)
    val methParamSyms = methParamProtos.map { param => methSym.newSyntheticValueParam(param.tpe, param.name.toTermName) }
    methSym setInfo MethodType(methParamSyms, resTp)

    // we must rewire reference to the function's param symbols -- and not methParamProtos -- to methParamSyms
    val useMethodParams = new TreeSymSubstituter(fun.vparams.map(_.symbol), methParamSyms)
    // we're now owned by the method that holds the body, and not the function
    val moveToMethod = new ChangeOwnerTraverser(fun.symbol, methSym)

    newDefDef(methSym, moveToMethod(useMethodParams(fun.body)))(tpt = TypeTree(resTp))
  }

  /**
    * Create a new `DefDef` based on `orig` with an explicit self parameter.
    *
    * Details:
    *   - Must by run after erasure
    *   - If `maybeClone` is the identity function, this runs "in place"
    *     and mutates the symbol of `orig`. `orig` should be discarded
    *   - Symbol owners and returns are substituted, as are parameter symbols
    *   - Recursive calls are not rewritten. This is correct if we assume
    *     that we either:
    *       - are in "in-place" mode, but can guarantee that no recursive calls exists
    *       - are associating the RHS with a cloned symbol, but intend for the original
    *         method to remain and for recursive calls to target it.
    */
  final def mkStatic(orig: DefDef, newName: Name, maybeClone: Symbol => Symbol): DefDef = {
    assert(phase.erasedTypes, phase)
    assert(!orig.symbol.hasFlag(SYNCHRONIZED), orig.symbol.defString)
    val origSym = orig.symbol
    val origParams = orig.symbol.info.params
    val newSym = maybeClone(orig.symbol)
    newSym.setName(newName)
    newSym.setFlag(STATIC)
    // Add an explicit self parameter
    val selfParamSym = newSym.newSyntheticValueParam(newSym.owner.typeConstructor, nme.SELF).setFlag(ARTIFACT)
    newSym.updateInfo(newSym.info match {
      case mt @ MethodType(params, res) => copyMethodType(mt, selfParamSym :: params, res)
    })
    val selfParam = ValDef(selfParamSym)
    val rhs = orig.rhs.substituteThis(newSym.owner, gen.mkAttributedIdent(selfParamSym)) // SD-186 intentionally leaving Ident($this) is unpositioned
      .substituteSymbols(origParams, newSym.info.params.drop(1)).changeOwner(origSym -> newSym)
    treeCopy.DefDef(orig, orig.mods, orig.name, orig.tparams, (selfParam :: orig.vparamss.head) :: Nil, orig.tpt, rhs).setSymbol(newSym)
  }

  def expandFunction(localTyper: analyzer.Typer)(fun: Function, inConstructorFlag: Long): Tree = {
    val anonClass = fun.symbol.owner newAnonymousFunctionClass(fun.pos, inConstructorFlag)
    val parents = if (isFunctionType(fun.tpe)) {
      anonClass addAnnotation SerialVersionUIDAnnotation
      addSerializable(abstractFunctionType(fun.vparams.map(_.symbol.tpe), fun.body.tpe.deconst))
    } else {
      if (fun.tpe.typeSymbol.isSubClass(JavaSerializableClass))
        anonClass addAnnotation SerialVersionUIDAnnotation
      fun.tpe :: Nil
    }
    anonClass setInfo ClassInfoType(parents, newScope, anonClass)

    // The original owner is used in the backend for the EnclosingMethod attribute. If fun is
    // nested in a value-class method, its owner was already changed to the extension method.
    // Saving the original owner allows getting the source structure from the class symbol.
    defineOriginalOwner(anonClass, fun.symbol.originalOwner)

    val samDef = mkMethodFromFunction(localTyper)(anonClass, fun)
    anonClass.info.decls enter samDef.symbol

    localTyper.typedPos(fun.pos) {
      Block(
        ClassDef(anonClass, NoMods, ListOfNil, List(samDef), fun.pos),
        Typed(New(anonClass.tpe), TypeTree(fun.tpe)))
    }
  }
}
