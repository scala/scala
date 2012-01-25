package scala.reflect
package macro

trait Context extends api.Universe {
  
  /** Mark a variable as captured; i.e. force boxing in a *Ref type.
   */
  def captureVariable(vble: Symbol): Unit
  
  /** Mark given identifier as a reference to a captured variable itself
   *  suppressing dereferencing with the `elem` field.
   */
  def referenceCapturedVariable(id: Ident): Tree

}
