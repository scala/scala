/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags._

/*
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Unapplies extends ast.TreeDSL
{
  self: Analyzer =>

  import global._
  import definitions._
  import CODE.{ CASE => _, _ }
  import treeInfo.{ isRepeatedParamType, isByNameParamType }

  private val unapplyParamName = nme.x_0


  // In the typeCompleter (templateSig) of a case class (resp it's module),
  // synthetic `copy` (reps `apply`, `unapply`) methods are added. To compute
  // their signatures, the corresponding ClassDef is needed. During naming (in
  // `enterClassDef`), the case class ClassDef is added as an attachment to the
  // moduleClass symbol of the companion module.
  class ClassForCaseCompanionAttachment(val caseClass: ClassDef)

  /** returns type list for return type of the extraction
   * @see extractorFormalTypes
   */
  def unapplyTypeList(ufn: Symbol, ufntpe: Type, nbSubPats: Int) = {
    assert(ufn.isMethod, ufn)
    //Console.println("utl "+ufntpe+" "+ufntpe.typeSymbol)
    ufn.name match {
      case nme.unapply | nme.unapplySeq =>
        val (formals, _) = extractorFormalTypes(unapplyUnwrap(ufntpe), nbSubPats, ufn)
        if (formals == null) throw new TypeError(s"$ufn of type $ufntpe cannot extract $nbSubPats sub-patterns")
        else formals
      case _ => throw new TypeError(ufn+" is not an unapply or unapplySeq")
    }
  }

  /** returns type of the unapply method returning T_0...T_n
   *  for n == 0, boolean
   *  for n == 1, Some[T0]
   *  else Some[Product[Ti]]
   */
  def unapplyReturnTypeExpected(argsLength: Int) = argsLength match {
    case 0 => BooleanClass.tpe
    case 1 => optionType(WildcardType)
    case n => optionType(productType((List fill n)(WildcardType)))
  }

  /** returns unapply or unapplySeq if available */
  def unapplyMember(tp: Type): Symbol = (tp member nme.unapply) match {
    case NoSymbol => tp member nme.unapplySeq
    case unapp    => unapp
  }
  /** returns unapply member's parameter type. */
  def unapplyParameterType(extractor: Symbol) = extractor.tpe.params match {
    case p :: Nil => p.tpe.typeSymbol
    case _        => NoSymbol
  }

  def copyUntyped[T <: Tree](tree: T): T =
    returning[T](tree.duplicate)(UnTyper traverse _)

  def copyUntypedInvariant(td: TypeDef): TypeDef = {
    val copy = treeCopy.TypeDef(td, td.mods &~ (COVARIANT | CONTRAVARIANT), td.name, td.tparams, td.rhs)

    returning[TypeDef](copy.duplicate)(UnTyper traverse _)
  }

  private def toIdent(x: DefTree) = Ident(x.name) setPos x.pos.focus

  private def classType(cdef: ClassDef, tparams: List[TypeDef], symbolic: Boolean = true): Tree = {
    val tycon = if (symbolic) REF(cdef.symbol) else Ident(cdef.name)
    if (tparams.isEmpty) tycon else AppliedTypeTree(tycon, tparams map toIdent)
  }

  private def constrParamss(cdef: ClassDef): List[List[ValDef]] = {
    val DefDef(_, _, _, vparamss, _, _) = treeInfo firstConstructor cdef.impl.body
    mmap(vparamss)(copyUntyped[ValDef])
  }

  /** The return value of an unapply method of a case class C[Ts]
   *  @param param  The name of the parameter of the unapply method, assumed to be of type C[Ts]
   *  @param caseclazz  The case class C[Ts]
   */
  private def caseClassUnapplyReturnValue(param: Name, caseclazz: Symbol) = {
    def caseFieldAccessorValue(selector: Symbol): Tree = Ident(param) DOT selector

    caseclazz.caseFieldAccessors match {
      case Nil      => TRUE
      case xs       => SOME(xs map caseFieldAccessorValue: _*)
    }
  }

  /** The module corresponding to a case class; overrides toString to show the module's name
   */
  def caseModuleDef(cdef: ClassDef): ModuleDef = {
    // > MaxFunctionArity is caught in Namers, but for nice error reporting instead of
    // an abrupt crash we trim the list here.
    def primaries      = constrParamss(cdef).head take MaxFunctionArity map (_.tpt)
    def inheritFromFun = !cdef.mods.hasAbstractFlag && cdef.tparams.isEmpty && constrParamss(cdef).length == 1
    def createFun      = gen.scalaFunctionConstr(primaries, toIdent(cdef), abstractFun = true)
    def parents        = if (inheritFromFun) List(createFun) else Nil
    def toString       = DefDef(
      Modifiers(OVERRIDE | FINAL),
      nme.toString_,
      Nil,
      ListOfNil,
      TypeTree(),
      Literal(Constant(cdef.name.decode)))

    companionModuleDef(cdef, parents, List(toString))
  }

  def companionModuleDef(cdef: ClassDef, parents: List[Tree] = Nil, body: List[Tree] = Nil): ModuleDef = atPos(cdef.pos.focus) {
    ModuleDef(
      Modifiers(cdef.mods.flags & AccessFlags | SYNTHETIC, cdef.mods.privateWithin),
      cdef.name.toTermName,
      Template(parents, emptyValDef, NoMods, Nil, body, cdef.impl.pos.focus))
  }

  private val caseMods = Modifiers(SYNTHETIC | CASE)

  /** The apply method corresponding to a case class
   */
  def factoryMeth(mods: Modifiers, name: TermName, cdef: ClassDef, symbolic: Boolean): DefDef = {
    val tparams   = cdef.tparams map copyUntypedInvariant
    val cparamss  = constrParamss(cdef)
    def classtpe = classType(cdef, tparams, symbolic)
    atPos(cdef.pos.focus)(
      DefDef(mods, name, tparams, cparamss, classtpe,
        New(classtpe, mmap(cparamss)(gen.paramToArg)))
    )
  }

  /** The apply method corresponding to a case class
   */
  def caseModuleApplyMeth(cdef: ClassDef): DefDef = factoryMeth(caseMods, nme.apply, cdef, symbolic = true)

  /** The unapply method corresponding to a case class
   */
  def caseModuleUnapplyMeth(cdef: ClassDef): DefDef = {
    val tparams   = cdef.tparams map copyUntypedInvariant
    val method    = constrParamss(cdef) match {
      case xs :: _ if xs.nonEmpty && isRepeatedParamType(xs.last.tpt) => nme.unapplySeq
      case _                                                          => nme.unapply
    }
    val cparams   = List(ValDef(Modifiers(PARAM | SYNTHETIC), unapplyParamName, classType(cdef, tparams), EmptyTree))
    val ifNull    = if (constrParamss(cdef).head.isEmpty) FALSE else REF(NoneModule)
    val body      = nullSafe({ case Ident(x) => caseClassUnapplyReturnValue(x, cdef.symbol) }, ifNull)(Ident(unapplyParamName))

    atPos(cdef.pos.focus)(
      DefDef(caseMods, method, tparams, List(cparams), TypeTree(), body)
    )
  }

  /**
   * Generates copy methods for case classes. Copy only has defaults on the first
   * parameter list, as of SI-5009.
   *
   * The parameter types of the copy method need to be exactly the same as the parameter
   * types of the primary constructor. Just copying the TypeTree is not enough: a type `C`
   * might refer to something else *inside* the class (i.e. as parameter type of `copy`)
   * than *outside* the class (i.e. in the class parameter list).
   *
   * One such example is t0054.scala:
   *   class A {
   *     case class B(x: C) extends A { def copy(x: C = x) = ... }
   *     class C {}      ^                          ^
   *   }                (1)                        (2)
   *
   * The reference (1) to C is `A.this.C`. The reference (2) is `B.this.C` - not the same.
   *
   * This is fixed with a hack currently. `Unapplies.caseClassCopyMeth`, which creates the
   * copy method, uses empty `TypeTree()` nodes for parameter types.
   *
   * In `Namers.enterDefDef`, the copy method gets a special type completer (`enterCopyMethod`).
   * Before computing the body type of `copy`, the class parameter types are assigned the copy
   * method parameters.
   *
   * This attachment class stores the copy method parameter ValDefs as an attachment in the
   * ClassDef of the case class.
   */
  def caseClassCopyMeth(cdef: ClassDef): Option[DefDef] = {
    def isDisallowed(vd: ValDef) = isRepeatedParamType(vd.tpt) || isByNameParamType(vd.tpt)
    val classParamss  = constrParamss(cdef)

    if (cdef.symbol.hasAbstractFlag || mexists(classParamss)(isDisallowed)) None
    else {
      def makeCopyParam(vd: ValDef, putDefault: Boolean) = {
        val rhs = if (putDefault) toIdent(vd) else EmptyTree
        val flags = PARAM | (vd.mods.flags & IMPLICIT) | (if (putDefault) DEFAULTPARAM else 0)
        // empty tpt: see comment above
        val tpt = atPos(vd.pos.focus)(TypeTree() setOriginal vd.tpt)
        treeCopy.ValDef(vd, Modifiers(flags), vd.name, tpt, rhs)
      }

      val tparams = cdef.tparams map copyUntypedInvariant
      val paramss = classParamss match {
        case Nil => Nil
        case ps :: pss =>
          ps.map(makeCopyParam(_, putDefault = true)) :: mmap(pss)(makeCopyParam(_, putDefault = false))
      }

      val classTpe = classType(cdef, tparams)
      val argss = mmap(paramss)(toIdent)
      val body: Tree = New(classTpe, argss)
      val copyDefDef = atPos(cdef.pos.focus)(
        DefDef(Modifiers(SYNTHETIC), nme.copy, tparams, paramss, TypeTree(), body)
      )
      Some(copyDefDef)
    }
  }
}
