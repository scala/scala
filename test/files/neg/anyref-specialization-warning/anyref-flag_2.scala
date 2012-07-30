/*
 * This test verifies the fact that the AnyRef specialization warning is only triggered for new code
 * Compile this file WITHOUT -Xanyref-specialization to get the warning, and it should only warn about
 * SpecTest2 but not SpecTest
 */
class SpecTest2[@specialized(Int, AnyRef) T, @specialized(Int) U](t: T, u: U) {
  val specTest = new SpecTest[T, U](t, u)
}
