/*
 * This test verifies the fact that there is an AnyRef specialization warning
 * Compile this file WITHOUT -Xanyref-specialization to get the warning
 */
class SpecTest[@specialized(Int, AnyRef) T, @specialized(Int) U](t: T, u: U)
