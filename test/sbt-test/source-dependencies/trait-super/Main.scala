class X extends E with C with B

object Main {

	def main(args: Array[String]): Unit = {
		val x = new X
		val expected = args(0).toInt
		assert(x.x == expected, "Expected " + expected + ", got " + x.x)
	}
}
