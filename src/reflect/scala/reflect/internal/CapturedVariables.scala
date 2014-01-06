package scala
package reflect
package internal

import Flags._

trait CapturedVariables { self: SymbolTable =>

  import definitions._

  /** Mark a variable as captured; i.e. force boxing in a *Ref type.
   */
  def captureVariable(vble: Symbol): Unit = vble setFlag CAPTURED

  /** Mark given identifier as a reference to a captured variable itself
   *  suppressing dereferencing with the `elem` field.
   */
  def referenceCapturedVariable(vble: Symbol): Tree = ReferenceToBoxed(Ident(vble))

  /** Convert type of a captured variable to *Ref type.
   */
  def capturedVariableType(vble: Symbol): Type =
    capturedVariableType(vble, NoType, erasedTypes = false)

  /** Convert type of a captured variable to *Ref type.
   */
  def capturedVariableType(vble: Symbol, tpe: Type = NoType, erasedTypes: Boolean = false): Type = {
    val tpe1 = if (tpe == NoType) vble.tpe else tpe
    val symClass = tpe1.typeSymbol
    def refType(valueRef: Map[Symbol, Symbol], objectRefClass: Symbol) =
      if (isPrimitiveValueClass(symClass) && symClass != UnitClass) valueRef(symClass).tpe
      else if (erasedTypes) objectRefClass.tpe
      else appliedType(objectRefClass, tpe1)
    if (vble.hasAnnotation(VolatileAttr)) refType(volatileRefClass, VolatileObjectRefClass)
    else refType(refClass, ObjectRefClass)
  }
}
