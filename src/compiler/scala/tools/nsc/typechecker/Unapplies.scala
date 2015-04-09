/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.reflect.internal.util.ListOfNil

/*
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Unapplies extends ast.TreeDSL {
  self: Analyzer =>

  import global._
  import definitions._
  import CODE.{ CASE => _, _ }
  import treeInfo.{ isRepeatedParamType, isByNameParamType }

  private def unapplyParamName = nme.x_0
  private def caseMods         = Modifiers(SYNTHETIC | CASE)

  // In the typeCompleter (templateSig) of a case class (resp it's module),
  // synthetic `copy` (reps `apply`, `unapply`) methods are added. To compute
  // their signatures, the corresponding ClassDef is needed. During naming (in
  // `enterClassDef`), the case class ClassDef is added as an attachment to the
  // moduleClass symbol of the companion module.
  class ClassForCaseCompanionAttachment(val caseClass: ClassDef)

  /** Returns unapply or unapplySeq if available, without further checks.
   */
  def directUnapplyMember(tp: Type): Symbol = (tp member nme.unapply) orElse (tp member nme.unapplySeq)

  /** Filters out unapplies with multiple (non-implicit) parameter lists,
   *  as they cannot be used as extractors
   */
  def unapplyMember(tp: Type): Symbol = directUnapplyMember(tp) filter (sym => !hasMultipleNonImplicitParamLists(sym))

  object HasUnapply {
    def unapply(tp: Type): Option[Symbol] = unapplyMember(tp).toOption
  }

  private def toIdent(x: DefTree) = Ident(x.name) setPos x.pos.focus

  private def classType(cdef: ClassDef, tparams: List[TypeDef]): Tree = {
    // SI-7033 Unattributed to avoid forcing `cdef.symbol.info`.
    val tycon = Ident(cdef.symbol)
    if (tparams.isEmpty) tycon else AppliedTypeTree(tycon, tparams map toIdent)
  }

  private def constrParamss(cdef: ClassDef): List[List[ValDef]] = {
    val ClassDef(_, _, _, Template(_, _, body)) = resetAttrs(cdef.duplicate)
    val DefDef(_, _, _, vparamss, _, _) = treeInfo firstConstructor body
    vparamss
  }

  private def constrTparamsInvariant(cdef: ClassDef): List[TypeDef] = {
    val ClassDef(_, _, tparams, _) = resetAttrs(cdef.duplicate)
    val tparamsInvariant = tparams.map(tparam => copyTypeDef(tparam)(mods = tparam.mods &~ (COVARIANT | CONTRAVARIANT)))
    tparamsInvariant
  }

  /** The return value of an unapply method of a case class C[Ts]
   *  @param param  The name of the parameter of the unapply method, assumed to be of type C[Ts]
   *  @param caseclazz  The case class C[Ts]
   */
  private def caseClassUnapplyReturnValue(param: Name, caseclazz: ClassDef) = {
    def caseFieldAccessorValue(selector: ValDef): Tree = {
      // Selecting by name seems to be the most straight forward way here to
      // avoid forcing the symbol of the case class in order to list the accessors.
      def selectByName = Ident(param) DOT caseAccessorName(caseclazz.symbol, selector.name)
      // But, that gives a misleading error message in neg/t1422.scala, where a case
      // class has an illegal private[this] parameter. We can detect this by checking
      // the modifiers on the param accessors.
      // We just generate a call to that param accessor here, which gives us an inaccessible
      // symbol error, as before.
      def localAccessor = caseclazz.impl.body find {
        case t @ ValOrDefDef(mods, selector.name, _, _) => mods.isPrivateLocal
        case _                                          => false
      }
      localAccessor.fold(selectByName)(Ident(param) DOT _.symbol)
    }

    // Working with trees, rather than symbols, to avoid cycles like SI-5082
    constrParamss(caseclazz).take(1).flatten match {
      case Nil => TRUE
      case xs  => SOME(xs map caseFieldAccessorValue: _*)
    }
  }

  /** The module corresponding to a case class; overrides toString to show the module's name
   */
  def caseModuleDef(cdef: ClassDef): ModuleDef = {
    val params = constrParamss(cdef)
    def inheritFromFun = !cdef.mods.hasAbstractFlag && cdef.tparams.isEmpty && (params match {
      case List(ps) if ps.length <= MaxFunctionArity => true
      case _ => false
    })
    def createFun = {
      def primaries = params.head map (_.tpt)
      gen.scalaFunctionConstr(primaries, toIdent(cdef), abstractFun = true)
    }

    def parents        = if (inheritFromFun) List(createFun) else Nil
    def toString       = DefDef(
      Modifiers(OVERRIDE | FINAL | SYNTHETIC),
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
      gen.mkTemplate(parents, noSelfType, NoMods, Nil, body, cdef.impl.pos.focus))
  }

  /** The apply method corresponding to a case class
   */
  def factoryMeth(mods: Modifiers, name: TermName, cdef: ClassDef): DefDef = {
    val tparams   = constrTparamsInvariant(cdef)
    val cparamss  = constrParamss(cdef)
    def classtpe = classType(cdef, tparams)
    atPos(cdef.pos.focus)(
      DefDef(mods, name, tparams, cparamss, classtpe,
        New(classtpe, mmap(cparamss)(gen.paramToArg)))
    )
  }

  /** The apply method corresponding to a case class
   */
  def caseModuleApplyMeth(cdef: ClassDef): DefDef = factoryMeth(caseMods, nme.apply, cdef)

  /** The unapply method corresponding to a case class
   */
  def caseModuleUnapplyMeth(cdef: ClassDef): DefDef = {
    val tparams    = constrTparamsInvariant(cdef)
    val method     = constrParamss(cdef) match {
      case xs :: _ if xs.nonEmpty && isRepeatedParamType(xs.last.tpt) => nme.unapplySeq
      case _                                                          => nme.unapply
    }
    val cparams    = List(ValDef(Modifiers(PARAM | SYNTHETIC), unapplyParamName, classType(cdef, tparams), EmptyTree))
    val resultType = if (!settings.isScala212) TypeTree() else { // fix for SI-6541 under -Xsource:2.12
    def repeatedToSeq(tp: Tree) = tp match {
        case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS_NAME), tps) => AppliedTypeTree(gen.rootScalaDot(tpnme.Seq), tps)
        case _                                                                => tp
      }
      constrParamss(cdef) match {
        case Nil | Nil :: _ =>
          gen.rootScalaDot(tpnme.Boolean)
        case params :: _ =>
          val constrParamTypes = params.map(param => repeatedToSeq(param.tpt))
          AppliedTypeTree(gen.rootScalaDot(tpnme.Option), List(treeBuilder.makeTupleType(constrParamTypes)))
      }
    }
    val ifNull     = if (constrParamss(cdef).head.isEmpty) FALSE else REF(NoneModule)
    val body       = nullSafe({ case Ident(x) => caseClassUnapplyReturnValue(x, cdef) }, ifNull)(Ident(unapplyParamName))

    atPos(cdef.pos.focus)(
      DefDef(caseMods, method, tparams, List(cparams), resultType, body)
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

      val tparams = constrTparamsInvariant(cdef)
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
