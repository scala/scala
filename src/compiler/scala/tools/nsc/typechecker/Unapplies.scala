/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

import scala.annotation.tailrec
import symtab.Flags._
import scala.reflect.internal.util.ListOfNil

/*
 *  @author  Martin Odersky
 */
trait Unapplies extends ast.TreeDSL {
  self: Analyzer =>

  import global._
  import definitions._
  import CODE.{ CASE => _, _ }
  import treeInfo.{ isRepeatedParamType, isByNameParamType }

  private def unapplyParamName = nme.x_0
  private def caseMods         = Modifiers(SYNTHETIC | CASE)

  // In the typeCompleter (templateSig) of a case class (resp its module),
  // synthetic `copy` (reps `apply`, `unapply`) methods are added. To compute
  // their signatures, the corresponding ClassDef is needed. During naming (in
  // `enterClassDef`), the case class ClassDef is added as an attachment to the
  // moduleClass symbol of the companion module.
  class ClassForCaseCompanionAttachment(val caseClass: ClassDef)

  /** Returns unapply or unapplySeq if available, without further checks.
   */
  def directUnapplyMember(tp: Type): Symbol = (tp member nme.unapply) orElse (tp member nme.unapplySeq)

  /** Filters out unapplies with invalid shapes: extractor methods must have
    * either one unary param list or one unary param list and an implicit param list.
   */
  def unapplyMember(tp: Type): Symbol = {
    def qualifies(sym: Symbol) =
      validateUnapplyMember(sym.info) == UnapplyMemberResult.Ok
    directUnapplyMember(tp) filter qualifies
  }

  // this slight extravagance opens this to reuse in error message generation
  object UnapplyMemberResult extends Enumeration {
    val Ok, NoParams, MultiParams, MultiParamss, VarArgs, Other = Value
  }
  @tailrec final def validateUnapplyMember(tp: Type): UnapplyMemberResult.Value = {
    import UnapplyMemberResult._
    tp match {
      case PolyType(_, restpe) => validateUnapplyMember(restpe)
      case MethodType(Nil, _) | NullaryMethodType(_) => NoParams
      case MethodType(_ :: Nil, snd: MethodType) =>
        if (snd.isImplicit) Ok else MultiParamss
      case MethodType(x :: Nil, _) =>
        if (definitions.isRepeated(x)) VarArgs
        else Ok
      case MethodType(_, _) => MultiParams
      case _ => Other
    }
  }

  object HasUnapply {
    def unapply(tp: Type): Option[Symbol] = unapplyMember(tp).toOption
  }

  private def toIdent(x: DefTree) = Ident(x.name) setPos x.pos.focus

  private def classType(cdef: ClassDef, tparams: List[TypeDef]): Tree = {
    // scala/bug#7033 Unattributed to avoid forcing `cdef.symbol.info`.
    val tycon = Ident(cdef.symbol)
    if (tparams.isEmpty) tycon else AppliedTypeTree(tycon, tparams map toIdent)
  }

  private def constrParamss(cdef: ClassDef): List[List[ValDef]] =
    resetAttrs(deriveClassDef(cdef)(deriveTemplate(_)(treeInfo.firstConstructor(_).duplicate :: Nil))) match {
      case ClassDef(_, _, _, Template(_, _, DefDef(_, _, _, vparamss, _, _) :: Nil)) => vparamss
      case x                                                                         => throw new MatchError(x)
    }

  private def constrTparamsInvariant(cdef: ClassDef): List[TypeDef] =
    resetAttrs(deriveClassDef(cdef)(_ => Template(Nil, noSelfType, Nil)).duplicate) match {
      case ClassDef(_, _, tparams, _) => tparams.map(tparam => copyTypeDef(tparam)(mods = tparam.mods &~ (COVARIANT | CONTRAVARIANT)))
      case x                          => throw new MatchError(x)
    }

  private def applyShouldInheritAccess(mods: Modifiers) =
    currentRun.isScala3 && (mods.hasFlag(PRIVATE) || (!mods.hasFlag(PROTECTED) && mods.hasAccessBoundary))

  /** The module corresponding to a case class; overrides toString to show the module's name
   */
  def caseModuleDef(cdef: ClassDef): ModuleDef = {
    val params = constrParamss(cdef)
    def inheritFromFun = !cdef.mods.hasAbstractFlag && cdef.tparams.isEmpty && (params match {
      case List(ps) if ps.length <= MaxFunctionArity => true
      case _ => false
    }) && !applyShouldInheritAccess(constrMods(cdef))
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


  private def constrMods(cdef: ClassDef): Modifiers = treeInfo.firstConstructorMods(cdef.impl.body)

  /** The apply method corresponding to a case class
   */
  def caseModuleApplyMeth(cdef: ClassDef): DefDef = {
    val inheritedMods = constrMods(cdef)
    val mods =
      if (applyShouldInheritAccess(inheritedMods))
        (caseMods | (inheritedMods.flags & PRIVATE)).copy(privateWithin = inheritedMods.privateWithin)
      else
        caseMods
    factoryMeth(mods, nme.apply, cdef)
  }

  /** The unapply method corresponding to a case class
   */
  def caseModuleUnapplyMeth(cdef: ClassDef): DefDef = {
    val tparams = constrTparamsInvariant(cdef)
    val vparamss = constrParamss(cdef)
    require(vparamss.nonEmpty, "case class must have params")
    val vparams = vparamss.head
    val booleanResult = vparamss.head.isEmpty
    val method = vparamss match {
      case xs :: _ if xs.nonEmpty && isRepeatedParamType(xs.last.tpt) => nme.unapplySeq
      case _                                                          => nme.unapply
    }
    val cparams = List(ValDef(Modifiers(PARAM | SYNTHETIC), unapplyParamName, classType(cdef, tparams), EmptyTree))
    def repeatedToSeq(tp: Tree) = tp match {
      case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS_NAME), tps) => AppliedTypeTree(gen.rootScalaDot(tpnme.Seq), tps)
      case _                                                                => tp
    }
    val resultType = // fix for scala/bug#6541 under -Xsource:2.12
      if (booleanResult) gen.rootScalaDot(tpnme.Boolean)
      else AppliedTypeTree(gen.rootScalaDot(tpnme.Option), List(treeBuilder.makeTupleType(vparams.map(p => repeatedToSeq(p.tpt)))))
    def selectCaseFieldAccessor(constrParam: ValDef): Tree = {
      val unapplyParam = Ident(unapplyParamName)

      // Selecting by name seems to be the most straight forward way here to
      // avoid forcing the symbol of the case class in order to list the accessors.
      //
      // But, that gives a misleading error message in neg/t1422.scala, where a case
      // class has an illegal private[this] parameter. We can detect this by checking
      // the modifiers on the param accessors.
      // We just generate a call to that param accessor here, which gives us an inaccessible
      // symbol error, as before.
      val accSel =
        cdef.impl.body collectFirst {
          case localAccessor@ValOrDefDef(mods, constrParam.name, _, _) if mods.isPrivateLocal => Select(unapplyParam, localAccessor.symbol)
        } getOrElse Select(unapplyParam, caseAccessorName(cdef.symbol, constrParam.name))

      constrParam.tpt match {
        case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS_NAME), tps) => Typed(accSel, AppliedTypeTree(gen.rootScalaDot(tpnme.Seq), tps))
        case _                                                                => accSel
      }
    }

    // Working with trees, rather than symbols, to avoid cycles like scala/bug#5082
    val body =
      If(Ident(unapplyParamName) OBJ_EQ NULL,
          if (booleanResult) FALSE else REF(NoneModule),
          if (booleanResult) TRUE else SOME(vparams.map(selectCaseFieldAccessor): _*)
        )

    atPos(cdef.pos.focus)(DefDef(caseMods, method, tparams, List(cparams), resultType, body))
  }

  /**
   * Generates copy methods for case classes. Copy only has defaults on the first
   * parameter list, as of scala/bug#5009.
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
      val copyMods =
        if (currentRun.isScala3) {
          val inheritedMods = constrMods(cdef)
          Modifiers(SYNTHETIC | (inheritedMods.flags & AccessFlags), inheritedMods.privateWithin)
        }
        else Modifiers(SYNTHETIC)
      val copyDefDef = atPos(cdef.pos.focus)(
        DefDef(copyMods, nme.copy, tparams, paramss, TypeTree(), body)
      )
      Some(copyDefDef)
    }
  }
}
