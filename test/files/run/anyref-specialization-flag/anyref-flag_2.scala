/*
 * This test verifies that code without AnyRef specialization correctly links to AnyRef-specialized code
 * Compile this file WITHOUT -Xanyref-specialization
 */
object Test extends App {
  println((new SpecTest(1, 2)).getClass.getSimpleName)   // SpecTest$mcII$sp
  println((new SpecTest("1", 2)).getClass.getSimpleName) // SpecTest$mcLI$sp
}
