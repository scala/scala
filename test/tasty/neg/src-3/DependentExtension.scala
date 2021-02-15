package tastytest

import DependentExtension.Box

// `DependentExtension` uses two type parameter lists in its `extract` method. the second type parameter list is
// dependent on the first term parameter list. It is unclear how to interpret this.
trait DependentExtension {
  extension [B <: Box](b: B) def extract[R >: b.Repr, O](f: R => O): O = f(b.value)
}

object DependentExtension {

  given DependentExtension with {}

  trait Box {
    type Repr
    val value: Repr
  }

}
