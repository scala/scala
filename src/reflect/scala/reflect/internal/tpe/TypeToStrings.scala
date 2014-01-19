package scala
package reflect
package internal
package tpe

import scala.collection.mutable.HashSet

private[internal] trait TypeToStrings {
  self: SymbolTable =>

  /** The maximum number of recursions allowed in toString
    */
  final val maxTostringRecursions = 50

  private var _tostringRecursions = 0
  def tostringRecursions = _tostringRecursions
  def tostringRecursions_=(value: Int) = _tostringRecursions = value

  private var _tostringSubjects = HashSet[Type]()
  def tostringSubjects = _tostringSubjects

  protected def typeToString(tpe: Type): String =
    if (tostringSubjects contains tpe) {
      // handles self-referential anonymous classes and who knows what else
      "..."
    }
    else if (tostringRecursions >= maxTostringRecursions) {
      devWarning("Exceeded recursion depth attempting to print " + util.shortClassOfInstance(tpe))
      if (settings.debug)
        (new Throwable).printStackTrace

      "..."
    }
    else
      try {
        tostringRecursions += 1
        tostringSubjects += tpe
        tpe.safeToString
      } finally {
        tostringSubjects -= tpe
        tostringRecursions -= 1
      }
}
