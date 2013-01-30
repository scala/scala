package scala.tools.nsc
package dependencies

import symtab.Flags

import scala.collection._

/** A component that describes the possible changes between successive
 *  compilations of a class.
 */
abstract class Changes {

  /** A compiler instance used to compile files on demand. */
  val compiler: Global

  import compiler._
  import symtab.Flags._

  abstract class Change

  private lazy val annotationsChecked =
    List(definitions.SpecializedClass) // Any others that should be checked?

  private val flagsToCheck = IMPLICIT | FINAL | PRIVATE | PROTECTED | SEALED |
                             OVERRIDE | CASE | ABSTRACT | DEFERRED | METHOD |
                             MODULE | INTERFACE | PARAM | BYNAMEPARAM | CONTRAVARIANT |
                             DEFAULTPARAM | ACCESSOR | LAZY | SPECIALIZED

  /** Are the new modifiers more restrictive than the old ones? */
  private def moreRestrictive(from: Long, to: Long): Boolean =
    ((((to & PRIVATE) != 0L) && (from & PRIVATE) == 0L)
     || (((to & PROTECTED) != 0L) && (from & PROTECTED) == 0L))

  /** Check if flags have changed **/
  private def modifiedFlags(from: Long, to: Long): Boolean =
    (from & IMPLICIT) != (to & IMPLICIT)

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
  case class ParentChanged(e: Entity) extends Change

  private val changedTypeParams = new mutable.HashSet[String]

  private def sameParameterSymbolNames(sym1: Symbol, sym2: Symbol): Boolean =
  	sameSymbol(sym1, sym2, true) || sym2.encodedName.startsWith(sym1.encodedName + nme.NAME_JOIN_STRING) // see #3140
  private def sameSymbol(sym1: Symbol, sym2: Symbol, simple: Boolean = false): Boolean =
    if (simple) sym1.encodedName == sym2.encodedName else sym1.fullName == sym2.fullName
  private def sameFlags(sym1: Symbol, sym2: Symbol): Boolean =
    	(sym1.flags & flagsToCheck) == (sym2.flags & flagsToCheck)
  private def sameAnnotations(sym1: Symbol, sym2: Symbol): Boolean =
    annotationsChecked.forall(a =>
      (sym1.hasAnnotation(a) == sym2.hasAnnotation(a)))

  private def sameType(tp1: Type, tp2: Type)(implicit strict: Boolean) = sameType0(tp1, tp2)

  private def sameType0(tp1: Type, tp2: Type)(implicit strict: Boolean): Boolean = ((tp1, tp2) match {
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
      val testSymbols =
        if (!sameSymbol(sym1, sym2)) {
          val v = (!strict && sym1.isType && sym2.isType && sameType(sym1.info, sym2.info))
          if (v) changedTypeParams += sym1.fullName
          v
        } else
          !sym1.isTypeParameter || !changedTypeParams.contains(sym1.fullName)

      testSymbols && sameType(pre1, pre2) &&
        (sym1.variance == sym2.variance) &&
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
    case (mt1 @ MethodType(params1, res1), mt2 @ MethodType(params2, res2)) =>
      // new dependent types: probably fix this, use substSym as done for PolyType
      sameTypes(tp1.paramTypes, tp2.paramTypes) &&
      (tp1.params corresponds tp2.params)((t1, t2) => sameParameterSymbolNames(t1, t2) && sameFlags(t1, t2)) &&
      sameType(res1, res2) &&
      mt1.isImplicit == mt2.isImplicit
    case (PolyType(tparams1, res1), PolyType(tparams2, res2)) =>
      sameTypeParams(tparams1, tparams2) && sameType(res1, res2)
    case (NullaryMethodType(res1), NullaryMethodType(res2)) =>
      sameType(res1, res2)
    case (ExistentialType(tparams1, res1), ExistentialType(tparams2, res2)) =>
      sameTypeParams(tparams1, tparams2)(false) && sameType(res1, res2)(false)
    case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) =>
      sameType(lo1, lo2) && sameType(hi1, hi2)
    case (BoundedWildcardType(bounds), _) =>
      bounds containsType tp2
    case (_, BoundedWildcardType(bounds)) =>
      bounds containsType tp1
    case (AnnotatedType(_,_,_), _) =>
      annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) &&
      sameType(tp1.withoutAnnotations, tp2.withoutAnnotations)
    case (_, AnnotatedType(_,_,_)) =>
      annotationsConform(tp1, tp2) && annotationsConform(tp2, tp1) &&
      sameType(tp1.withoutAnnotations, tp2.withoutAnnotations)
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

  private def sameTypeParams(tparams1: List[Symbol], tparams2: List[Symbol])(implicit strict: Boolean) =
    sameTypes(tparams1 map (_.info), tparams2 map (_.info)) &&
    sameTypes(tparams1 map (_.tpe), tparams2 map (_.tpe)) &&
    (tparams1 corresponds tparams2)((t1, t2) => sameAnnotations(t1, t2))

  private def sameTypes(tps1: List[Type], tps2: List[Type])(implicit strict: Boolean) =
    (tps1 corresponds tps2)(sameType(_, _))

  /** Return the list of changes between 'from' and 'toSym.info'.
   */
  def changeSet(from: Type, toSym: Symbol): List[Change] = {
    implicit val defaultStrictTypeRefTest = true

    val to = toSym.info
    changedTypeParams.clear
    def omitSymbols(s: Symbol): Boolean = !s.hasFlag(LOCAL | LIFTED | PRIVATE | SYNTHETIC)
    val cs = new mutable.ListBuffer[Change]

    if ((from.parents zip to.parents) exists { case (t1, t2) => !sameType(t1, t2) })
      cs += Changed(toEntity(toSym))(from.parents.zip(to.parents).toString)
    if (!sameTypeParams(from.typeParams, to.typeParams)(false))
      cs += Changed(toEntity(toSym))(" tparams: " + from.typeParams.zip(to.typeParams))

    // new members not yet visited
    val newMembers = mutable.HashSet[Symbol]()
    newMembers ++= to.decls.iterator filter omitSymbols

    for (o <- from.decls.iterator filter omitSymbols) {
      val n = to.decl(o.name)
      newMembers -= n

      if (o.isClass)
        cs ++= changeSet(o.info, n)
      else if (n == NoSymbol)
        cs += Removed(toEntity(o))
      else {
        val newSym =
            o match {
              case _:TypeSymbol if o.isAliasType =>
                n.suchThat(ov => sameType(ov.info, o.info))
              case _                             =>
                n.suchThat(ov => sameType(ov.tpe, o.tpe))
             }
        if (newSym == NoSymbol || moreRestrictive(o.flags, newSym.flags) || modifiedFlags(o.flags, newSym.flags))
          cs += Changed(toEntity(o))(n + " changed from " + o.tpe + " to " + n.tpe + " flags: " + Flags.flagsToString(o.flags))
        else if (newSym.isGetter && (o.accessed(from).hasFlag(MUTABLE) != newSym.accessed.hasFlag(MUTABLE)))
          // o.owner is already updated to newSym.owner
          // so o.accessed will return the accessed for the new owner
          cs += Changed(toEntity(o))(o.accessed(from) + " changed to " + newSym.accessed)
        else
          newMembers -= newSym
      }
    }: Unit // Give the type explicitly until #2281 is fixed

    cs ++= (newMembers map (Added compose toEntity))
    cs.toList
  }
  def removeChangeSet(sym: Symbol): Change = Removed(toEntity(sym))
  def changeChangeSet(sym: Symbol, msg: String): Change = Changed(toEntity(sym))(msg)
  def parentChangeSet(sym: Symbol): Change = ParentChanged(toEntity(sym))

  private def toEntity(sym: Symbol): Entity =
    if (sym.isClass) Class(sym.fullName)
    else Definition(sym.fullName)
}
