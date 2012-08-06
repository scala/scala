/*
 * Compile this file WITH -Xanyref-specialization
 */
class SpecTest[@specialized(Int, AnyRef) T, @specialized(Int) U](t: T, u: U)
