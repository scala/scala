package tastytest

import collection.mutable

/** This blows up as example refers to an object with no field called map */
object TestTraitWithSideEffects extends Suite("TestTraitWithSideEffects") {
  val example = new TraitWithSideEffects {}
  test(assert(TraitWithSideEffects.verify(example) === true))
}
