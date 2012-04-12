package scala.reflect.makro

trait Names {
  self: Context =>

  /** Creates a fresh string */
  def fresh(): String

  /** Creates a fresh string from the provided string */
  def fresh(name: String): String

  /** Creates a fresh name from the provided name */
  def fresh(name: Name): Name
}
