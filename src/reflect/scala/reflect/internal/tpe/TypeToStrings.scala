package scala
package reflect
package internal
package tpe

import scala.collection.mutable.HashSet

private[internal] trait TypeToStrings {
  self: SymbolTable =>

  /** The maximum number of recursions allowed in toString
    */
  final val maxToStringRecursions = 50

  private var _toStringRecursions = 0
  def toStringRecursions = _toStringRecursions
  def toStringRecursions_=(value: Int) = _toStringRecursions = value

  private var _toStringSubjects = HashSet[Type]()
  def toStringSubjects = _toStringSubjects

  protected def typeToString(tpe: Type): String =
    if (toStringSubjects contains tpe) {
      // handles self-referential anonymous classes and who knows what else
      "..."
    }
    else if (toStringRecursions >= maxToStringRecursions) {
      devWarning("Exceeded recursion depth attempting to print " + util.shortClassOfInstance(tpe))
      if (settings.debug)
        (new Throwable).printStackTrace

      "..."
    }
    else
      try {
        toStringRecursions += 1
        toStringSubjects += tpe
        tpe.safeToString
      } finally {
        toStringSubjects -= tpe
        toStringRecursions -= 1
      }
}
