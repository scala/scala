package scala.reflect
package makro

trait CapturedVariables {
  self: Context =>

  import mirror._

  /** Mark a variable as captured; i.e. force boxing in a *Ref type.
   */
  def captureVariable(vble: Symbol): Unit

  /** Mark given identifier as a reference to a captured variable itself
   *  suppressing dereferencing with the `elem` field.
   */
  def referenceCapturedVariable(vble: Symbol): Tree

  /** Convert type of a captured variable to *Ref type.
   */
  def capturedVariableType(vble: Symbol): Type
}