package scala.reflect.makro
package runtime

trait CapturedVariables {
  self: Context =>

  import mirror._
  import universe._

  def captureVariable(vble: Symbol): Unit = universe.captureVariable(vble)

  def referenceCapturedVariable(vble: Symbol): Tree = universe.referenceCapturedVariable(vble)

  def capturedVariableType(vble: Symbol): Type = universe.capturedVariableType(vble)
}