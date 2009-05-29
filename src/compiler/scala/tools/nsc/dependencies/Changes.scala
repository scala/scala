package scala.tools.nsc.dependencies

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

  case class Parents(target: Symbol) extends Change
  case class TypeParams(from: List[Symbol], to: List[Symbol]) extends Change
  case class Added(member: String) extends Change
  case class Removed(member: String) extends Change
  case class Changed(member: String)extends Change


  /** Return the list of changes between 'from' and 'to'.
   */
  def changeSet(from: Symbol, to: Symbol): List[Change] = {
    val cs = new mutable.ListBuffer[Change]

    if ((from.info.parents intersect to.info.parents).size
        != from.info.parents.size)
      cs += Parents(from)

    if (from.typeParams != to.typeParams)
      cs += TypeParams(from.typeParams, to.typeParams)

    // new members not yet visited
    val newMembers = mutable.HashSet[String]()
    (to.info.decls.elements) foreach (newMembers += _.fullNameString)

    for (o <- from.info.decls.elements;
         val n = to.info.decl(o.name)) {
      newMembers -= n.fullNameString

      if (n == NoSymbol)
        cs += Removed(o.fullNameString)
      else if (n.suchThat(_.tpe == o.tpe) == NoSymbol)
        cs += Changed(o.fullNameString)
    }
    cs ++= (newMembers map Added)

    cs.toList
  }

}
