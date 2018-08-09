package scala

package object util {
  /**
   * Adds chaining methods `tap` and `pipe` to every type. See [[ChainingOps]].
   */
  object chaining extends ChainingSyntax
}
