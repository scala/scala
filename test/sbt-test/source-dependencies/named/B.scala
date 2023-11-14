object B {
	def main(args: Array[String]): Unit =
	{
		val result = A.x(zz = 3, yy = 4)
		assert(result == args(0).toInt, result)
	}
}
