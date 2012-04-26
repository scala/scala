package scala.reflect.makro
package runtime

trait CapturedVariables {
  self: Context =>

  import mirror._

  def captureVariable(vble: Symbol): Unit = mirror.captureVariable(vble)

  def referenceCapturedVariable(vble: Symbol): Tree = mirror.referenceCapturedVariable(vble)

  def capturedVariableType(vble: Symbol): Type = mirror.capturedVariableType(vble)
}