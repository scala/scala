package scala.reflect.makro
package runtime

trait Names {
  self: Context =>

  lazy val freshNameCreator = callsiteTyper.context.unit.fresh

  def fresh(): String = {
    freshNameCreator.newName()
  }

  def fresh(name: String): String = {
    freshNameCreator.newName(name)
  }

  def fresh(name: Name): Name = {
    name.mapName(freshNameCreator.newName(_))
  }
}