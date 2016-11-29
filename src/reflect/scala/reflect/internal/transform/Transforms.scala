package scala
package reflect
package internal
package transform

import scala.language.existentials

trait Transforms { self: SymbolTable =>

  /** We need to encode laziness by hand here because the three components refChecks, uncurry and erasure
   *  are overwritten by objects in Global.
   *  It would be best of objects could override lazy values. See SI-5187.
   *  In the absence of this, the Lazy functionality should probably be somewhere
   *  in the standard library. Or is it already?
   */
  private class Lazy[T](op: => T) {
    private var value: T = _
    private var _isDefined = false
    def isDefined = _isDefined
    def force: T = {
      if (!isDefined) { value = op; _isDefined = true }
      value
    }
  }

  private val uncurryLazy     = new Lazy(new { val global: Transforms.this.type = self } with UnCurry)
  private val erasureLazy     = new Lazy(new { val global: Transforms.this.type = self } with Erasure)
  private val postErasureLazy = new Lazy(new { val global: Transforms.this.type = self } with PostErasure)

  def uncurry = uncurryLazy.force
  def erasure = erasureLazy.force
  def postErasure = postErasureLazy.force

  def transformedType(sym: Symbol) =
    postErasure.transformInfo(sym,
      erasure.transformInfo(sym,
        uncurry.transformInfo(sym, sym.info)))

  def transformedType(tpe: Type) =
    postErasure.elimErasedValueType(erasure.scalaErasure(uncurry.uncurry(tpe)))

}
