/*
 * Tests if two lambdas defined in the same class do not lead to
 * name clashes.
 */
object Test {
	def takeLambda(f: Int => Int ): Int = f(12)

	def main(args: Array[String]): Unit = {
		println(takeLambda(x => x+1))
		println(takeLambda(x => x*2))
	}
}
