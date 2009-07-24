package scala.tools.nsc
package dependencies

import symtab.Flags

import collection._

/** A component that describes the possible changes between successive
 *  compilations of a class.
 */
abstract class Changes {

  /** A compiler instance used to compile files on demand. */
  val compiler: Global

  import compiler._
  import symtab.Flags._

  abstract class Change

  /** Modifiers of target have changed */
  case class Mods(from: Long, to: Long)(target: Symbol) extends Change {
    def moreRestrictive: Boolean =
      ((((to & PRIVATE) != 0) && (from & PRIVATE) == 0)
        || (((to & PROTECTED) != 0) && (from & PROTECTED) == 0))

    def morePermissive: Boolean = !moreRestrictive
  }


  /** An entity in source code, either a class or a member definition.
   *  Name is fully-qualified.
   */
  abstract class Entity
  case class Class(name: String) extends Entity
  case class Definition(name: String) extends Entity

  case class Added(e: Entity) extends Change
  case class Removed(e: Entity) extends Change
  case class Changed(e: Entity)(implicit val reason: String) extends Change {
    override def toString = "Changed(" + e + ")[" + reason + "]"
  }

  private def sameSymbol(sym1: Symbol, sym2: Symbol): Boolean =
    sym1.fullNameString == sym2.fullNameString

  private def sameType(tp1: Type, tp2: Type) = {
    def typeOf(tp: Type): String = tp.toString + "[" + tp.getClass + "]"
    val res = sameType0(tp1, tp2)
//    if (!res) println("\t different types: " + typeOf(tp1) + " : " + typeOf(tp2))
    res
  }

  private def sameType0(tp1: Type, tp2: Type): Boolean = ((tp1, tp2) match {
    /*case (ErrorType, _) => false
    case (WildcardType, _) => false
    case (_, ErrorType) => false
    case (_, WildcardType) => false
    */
    case (NoType, _) => false
    case (NoPrefix, NoPrefix) => true
    case (_, NoType) => false
    case (_, NoPrefix) => false

    case (ThisType(sym1), ThisType(sym2))
      if sameSymbol(sym1, sym2) => true

    case (SingleType(pre1, sym1), SingleType(pre2, sym2))
      if sameType(pre1, pre2) && sameSymbol(sym1, sym2) => true
    case (ConstantType(value1), ConstantType(value2)) =>
      value1 == value2
    case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
      sameType(pre1, pre2) && sameSymbol(sym1, sym2) &&
      ((tp1.isHigherKinded && tp2.isHigherKinded && tp1.normalize =:= tp2.normalize) ||
         sameTypes(args1, args2))
         // @M! normalize reduces higher-kinded case to PolyType's

    case (RefinedType(parents1, ref1), RefinedType(parents2, ref2)) =>
      def isSubScope(s1: Scope, s2: Scope): Boolean = s2.toList.forall {
        sym2 =>
          var e1 = s1.lookupEntry(sym2.name)
          (e1 ne null) && {
            var isEqual = false
            while (!isEqual && (e1 ne null)) {
              isEqual = sameType(e1.sym.info, sym2.info)
              e1 = s1.lookupNextEntry(e1)
            }
            isEqual
          }
      }
      sameTypes(parents1, parents2) && isSubScope(ref1, ref2) && isSubScope(ref2, ref1)

    case (MethodType(params1, res1), MethodType(params2, res2)) =>
      // new dependent types: probably fix this, use substSym as done for PolyType
      (sameTypes(tp1.paramTypes, tp2.paramTypes) &&
       sameType(res1, res2) &&
       tp1.isInstanceOf[ImplicitMethodType] == tp2.isInstanceOf[ImplicitMethodType])

    case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
      (tparams1.length == tparams2.length &&
       List.forall2(tparams1, tparams2)
       ((p1, p2) => sameType(p1.info, p2.info)) &&
       sameType(res1, res2))
    case (ExistentialType(tparams1, res1), ExistentialType(tparams2, res2)) =>
      (tparams1.length == tparams2.length &&
       List.forall2(tparams1, tparams2)
       ((p1, p2) => sameType(p1.info, p2.info)) &&
       sameType(res1, res2))
    case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
        sameType(lo1, lo2) && sameType(hi1, hi2)
    case (BoundedWildcardType(bounds), _) =>
      bounds containsType tp2
    case (_, BoundedWildcardType(bounds)) =>
      bounds containsType tp1

    case (AnnotatedType(_,_,_), _) =>
      annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && tp1.withoutAnnotations =:= tp2.withoutAnnotations
    case (_, AnnotatedType(_,_,_)) =>
      annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) && tp1.withoutAnnotations =:= tp2.withoutAnnotations

    case (_: SingletonType, _: SingletonType) =>
      var origin1 = tp1
      while (origin1.underlying.isInstanceOf[SingletonType]) {
        assert(origin1 ne origin1.underlying, origin1)
        origin1 = origin1.underlying
      }
      var origin2 = tp2
      while (origin2.underlying.isInstanceOf[SingletonType]) {
        assert(origin2 ne origin2.underlying, origin2)
        origin2 = origin2.underlying
      }
      ((origin1 ne tp1) || (origin2 ne tp2)) && sameType(origin1, origin2)
    case _ =>
      false
    }) || {
      val tp1n = normalizePlus(tp1)
      val tp2n = normalizePlus(tp2)
      ((tp1n ne tp1) || (tp2n ne tp2)) && sameType(tp1n, tp2n)
    }

  def sameTypes(tps1: List[Type], tps2: List[Type]): Boolean =
    (tps1.length == tps2.length
     && List.forall2(tps1, tps2)(sameType))

  /** Return the list of changes between 'from' and 'to'.
   */
  def changeSet(from: Symbol, to: Symbol): List[Change] = {
    implicit val defaultReason = "types"
//     println("changeSet " + from + "(" + from.info + ")"
//             + " vs " + to + "(" + to.info + ")")
    val cs = new mutable.ListBuffer[Change]

    if ((from.info.parents zip to.info.parents) exists { case (t1, t2) => !sameType(t1, t2) })
      cs += Changed(toEntity(from))(from.info.parents.zip(to.info.parents).toString)
    if (from.typeParams != to.typeParams)
      cs += Changed(toEntity(from))(" tparams: " + from.typeParams.zip(to.typeParams))

    // new members not yet visited
    val newMembers = mutable.HashSet[Symbol]()
    newMembers ++= to.info.decls.elements

    for (o <- from.info.decls.elements;
         val n = to.info.decl(o.name)) {
      newMembers -= n

      if (!o.hasFlag(Flags.PRIVATE | Flags.LOCAL | Flags.LIFTED)) {
        if (o.isClass)
          cs ++= changeSet(o, n)
        else if (n == NoSymbol)
          cs += Removed(toEntity(o))
        else {
          val newSym = n.suchThat(ov => sameType(ov.tpe, o.tpe))
          if (newSym == NoSymbol) {
            cs += Changed(toEntity(o))(n + " changed from " + o.tpe + " to " + n.tpe + " flags: " + Flags.flagsToString(o.flags))
          } else
            newMembers -= newSym
        }
      }
    }
    cs ++= (newMembers map (Added compose toEntity))

    cs.toList
  }

  private def toEntity(sym: Symbol): Entity =
    if (sym.isClass) Class(sym.fullNameString)
    else Definition(sym.fullNameString)
}
