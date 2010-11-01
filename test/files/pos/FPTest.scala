// On some hypothetical future day when we can test the emitted bytecode,
// should look for the fp bit.  Until then, just a pos test.
import annotation.strictfp

@strictfp class FPTest {
  def main(args: Array[String]): Unit = {
    val d: Double = 8e+307
    println(4.0 * d * 0.5);
    println(2.0 * d);
  }
}
