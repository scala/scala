package scala.reflect
package internal
package tpe

private[internal] trait TypeToStrings {
  self: SymbolTable =>

  /** The maximum number of recursions allowed in toString
   */
  final val maxTostringRecursions = 50

  private var tostringRecursions = 0

  protected def typeToString(tpe: Type): String =
    if (tostringRecursions >= maxTostringRecursions) {
      debugwarn("Exceeded recursion depth attempting to print type.")
      if (settings.debug.value)
        (new Throwable).printStackTrace

      "..."
    }
    else
      try {
        tostringRecursions += 1
        tpe.safeToString
      } finally {
        tostringRecursions -= 1
      }
}
