/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
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

  /** returns type list for return type of the extraction */
  def unapplyTypeList(ufn: Symbol, ufntpe: Type) = {
    assert(ufn.isMethod, ufn)
    //Console.println("utl "+ufntpe+" "+ufntpe.typeSymbol)
    ufn.name match {
      case nme.unapply    => unapplyTypeListFromReturnType(ufntpe)
      case nme.unapplySeq => unapplyTypeListFromReturnTypeSeq(ufntpe)
      case _              => throw new TypeError(ufn+" is not an unapply or unapplySeq")
    }
  }
  /** (the inverse of unapplyReturnTypeSeq)
   *  for type Boolean, returns Nil
   *  for type Option[T] or Some[T]:
   *   - returns T0...Tn if n>0 and T <: Product[T0...Tn]]
   *   - returns T otherwise
   */
  def unapplyTypeListFromReturnType(tp1: Type): List[Type] = {
    val tp = unapplyUnwrap(tp1)
    tp.typeSymbol match {                             // unapplySeqResultToMethodSig
      case BooleanClass             => Nil
      case OptionClass | SomeClass  =>
        val prod  = tp.typeArgs.head
// the spec doesn't allow just any subtype of Product, it *must* be TupleN[...] -- see run/virtpatmat_extends_product.scala
// this breaks plenty of stuff, though...
//        val targs =
//          if (isTupleType(prod)) getProductArgs(prod)
//          else List(prod)
        val targs = getProductArgs(prod)

        if (targs.isEmpty || targs.tail.isEmpty) List(prod) // special n == 0 ||  n == 1
        else targs  // n > 1
      case _                        =>
        throw new TypeError("result type "+tp+" of unapply not in {Boolean, Option[_], Some[_]}")
    }
  }

  /** let type be the result type of the (possibly polymorphic) unapply method
   *  for type Option[T] or Some[T]
   *  -returns T0...Tn-1,Tn* if n>0 and T <: Product[T0...Tn-1,Seq[Tn]]],
   *  -returns R* if T = Seq[R]
   */
  def unapplyTypeListFromReturnTypeSeq(tp1: Type): List[Type] = {
    val tp = unapplyUnwrap(tp1)
    tp.typeSymbol match {
      case OptionClass | SomeClass  =>
        val ts = unapplyTypeListFromReturnType(tp1)
        val last1 = (ts.last baseType SeqClass) match {
          case TypeRef(pre, SeqClass, args) => typeRef(pre, RepeatedParamClass, args)
          case _                            => throw new TypeError("last not seq")
        }
        ts.init :+ last1
      case _                        =>
        throw new TypeError("result type "+tp+" of unapply not in {Option[_], Some[_]}")
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
      List(Nil),
      TypeTree(),
      Literal(Constant(cdef.name.decode)))

    companionModuleDef(cdef, parents, List(toString))
  }

  def companionModuleDef(cdef: ClassDef, parents: List[Tree] = Nil, body: List[Tree] = Nil): ModuleDef = atPos(cdef.pos.focus) {
    ModuleDef(
      Modifiers(cdef.mods.flags & AccessFlags | SYNTHETIC, cdef.mods.privateWithin),
      cdef.name.toTermName,
      Template(parents, emptyValDef, NoMods, Nil, List(Nil), body, cdef.impl.pos.focus))
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

  def caseClassCopyMeth(cdef: ClassDef): Option[DefDef] = {
    def isDisallowed(vd: ValDef) = isRepeatedParamType(vd.tpt) || isByNameParamType(vd.tpt)
    val cparamss  = constrParamss(cdef)
    val flat      = cparamss.flatten

    if (cdef.symbol.hasAbstractFlag || (flat exists isDisallowed)) None
    else {
      val tparams = cdef.tparams map copyUntypedInvariant
      // the parameter types have to be exactly the same as the constructor's parameter types; so it's
      // not good enough to just duplicated the (untyped) tpt tree; the parameter types are removed here
      // and re-added in ``finishWith'' in the namer.
      def paramWithDefault(vd: ValDef) =
        treeCopy.ValDef(vd, vd.mods | DEFAULTPARAM, vd.name, atPos(vd.pos.focus)(TypeTree() setOriginal vd.tpt), toIdent(vd))
        
      val (copyParamss, funParamss) = cparamss match {
        case Nil => (Nil, Nil)
        case ps :: pss =>
          (List(ps.map(paramWithDefault)), mmap(pss)(p => copyUntyped[ValDef](p).copy(rhs = EmptyTree)))
      }

      val classTpe = classType(cdef, tparams)
      val bodyTpe = funParamss.foldRight(classTpe)((params, restp) => gen.scalaFunctionConstr(params.map(_.tpt), restp))

      val argss = copyParamss match {
        case Nil       => Nil
        case ps :: Nil => mmap(ps :: funParamss)(toIdent)
      }      
      val body = funParamss.foldRight(New(classTpe, argss): Tree)(Function)

      Some(atPos(cdef.pos.focus)(
        DefDef(Modifiers(SYNTHETIC), nme.copy, tparams, copyParamss, bodyTpe,
          body)
      ))
    }
  }
}
