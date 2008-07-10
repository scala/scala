trait A
{
	private[this] val p = 5

	def f = (b: Byte) => p
}

trait B
{
	def failure: Boolean
	def success = !failure
}

