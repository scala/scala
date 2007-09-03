/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: EtaExpansion.scala 12607 2007-08-21 14:04:16Z odersky $

package scala.tools.nsc.typechecker

/*
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait Unapplies { self: Analyzer =>

  import global._
  import definitions._

  /** returns type list for return type of the extraction */
  def unapplyTypeList(ufn: Symbol, ufntpe: Type) = {
    assert(ufn.isMethod)
    //Console.println("utl "+ufntpe+" "+ufntpe.typeSymbol)
    ufn.name match {
      case nme.unapply    => unapplyTypeListFromReturnType(ufntpe)
      case nme.unapplySeq => unapplyTypeListFromReturnTypeSeq(ufntpe)
      case _ => throw new TypeError(ufn+" is not an unapply or unapplySeq")
    }
  }
  /** (the inverse of unapplyReturnTypeSeq)
   *  for type Boolean, returns Nil
   *  for type Option[T] or Some[T]:
   *   - returns T0...Tn if n>0 and T <: Product[T0...Tn]]
   *   - returns T otherwise
   */
  def unapplyTypeListFromReturnType(tp1: Type): List[Type] =  { // rename: unapplyTypeListFromReturnType
    val tp = unapplyUnwrap(tp1)
    val B = BooleanClass
    val O = OptionClass
    val S = SomeClass
    tp.typeSymbol match { // unapplySeqResultToMethodSig
      case  B => Nil
      case  O | S =>
        val prod = tp.typeArgs.head
        getProductArgs(prod)  match {
          case Some(all @ (x1::x2::xs)) => all       // n >= 2
          case _                        => prod::Nil // special n == 0 ||  n == 1
        }
      case _ => throw new TypeError("result type "+tp+" of unapply not in {boolean, Option[_], Some[_]}")
    }
  }

  /** let type be the result type of the (possibly polymorphic) unapply method
   *  for type Option[T] or Some[T]
   *  -returns T0...Tn-1,Tn* if n>0 and T <: Product[T0...Tn-1,Seq[Tn]]],
   *  -returns R* if T = Seq[R]
   */
  def unapplyTypeListFromReturnTypeSeq(tp1: Type): List[Type] = {
    val tp = unapplyUnwrap(tp1)
    val   O = OptionClass; val S = SomeClass; tp.typeSymbol match {
    case  O                  | S =>
      val ts = unapplyTypeListFromReturnType(tp1)
      val last1 = ts.last.baseType(SeqClass) match {
        case TypeRef(pre, seqClass, args) => typeRef(pre, RepeatedParamClass, args)
        case _ => throw new TypeError("last not seq")
      }
      ts.init ::: List(last1)
      case _ => throw new TypeError("result type "+tp+" of unapply not in {Option[_], Some[_]}")
    }
  }

  /** returns type of the unapply method returning T_0...T_n
   *  for n == 0, boolean
   *  for n == 1, Some[T0]
   *  else Some[Product[Ti]]
  def unapplyReturnType(elems: List[Type], useWildCards: Boolean) =
    if (elems.isEmpty)
      BooleanClass.tpe
    else if (elems.length == 1)
      optionType(if(useWildCards) WildcardType else elems(0))
    else
      productType({val es = elems; if(useWildCards) elems map { x => WildcardType} else elems})
   */

  def unapplyReturnTypeExpected(argsLength: Int) = argsLength match {
    case 0 => BooleanClass.tpe
    case 1 => optionType(WildcardType)
    case n => optionType(productType(List.range(0,n).map (arg => WildcardType)))
  }

  /** returns unapply or unapplySeq if available */
  def unapplyMember(tp: Type): Symbol = {
    var unapp = tp.member(nme.unapply)
    if (unapp == NoSymbol) unapp = tp.member(nme.unapplySeq)
    unapp
  }
}
