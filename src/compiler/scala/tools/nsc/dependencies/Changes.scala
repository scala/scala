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


  /** An entity in source code, either a class or a member definition.
   *  Name is fully-qualified.
   */
  abstract class Entity
  case class Class(name: String) extends Entity
  case class Definition(name: String) extends Entity

  case class Added(e: Entity) extends Change
  case class Removed(e: Entity) extends Change
  case class Changed(e: Entity) extends Change

  /** Return the list of changes between 'from' and 'to'.
   */
  def changeSet(from: Symbol, to: Symbol): List[Change] = {
//     println("changeSet " + from + "(" + from.info + ")"
//             + " vs " + to + "(" + to.info + ")")
    val cs = new mutable.ListBuffer[Change]

    if (((from.info.parents zip to.info.parents) exists { case (t1, t2) => !(t1 =:= t2) })
        || (from.typeParams != to.typeParams))
      cs += Changed(toEntity(from))

    // new members not yet visited
    val newMembers = mutable.HashSet[Symbol]()
    newMembers ++= to.info.decls.elements

    for (o <- from.info.decls.elements;
         val n = to.info.decl(o.name)) {
      newMembers -= n

      if (o.isClass)
        cs ++= changeSet(o, n)
      else if (n == NoSymbol)
        cs += Removed(toEntity(o))
      else {
        val newSym = n.suchThat(_.tpe =:= o.tpe)
        if (newSym == NoSymbol) {
//          println(n + " changed from " + o.tpe + " to " + n.tpe)
          cs += Changed(toEntity(o))
        } else
          newMembers -= newSym
      }
    }
    cs ++= (newMembers map (Added compose toEntity))

    cs.toList
  }

  private def toEntity(sym: Symbol): Entity =
    if (sym.isClass) Class(sym.fullNameString)
    else Definition(sym.fullNameString)
}
