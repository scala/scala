package scala.reflect.macros
package contexts

trait Names {
  self: Context =>

  lazy val freshNameCreator = callsiteTyper.context.unit.fresh

  def fresh(): String =
    freshName()

  def fresh(name: String): String =
    freshName(name)

  def fresh[NameType <: Name](name: NameType): NameType =
    freshName[NameType](name)

  def freshName(): String =
    freshNameCreator.newName()

  def freshName(name: String): String =
    freshNameCreator.newName(name)

  def freshName[NameType <: Name](name: NameType): NameType =
    name.mapName(freshNameCreator.newName(_)).asInstanceOf[NameType]
}