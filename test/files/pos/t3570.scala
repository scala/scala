class test {
	object Break extends Throwable
	def break = throw Break
	def block(x: => Unit): Unit = {
		try { x } catch { case e: Break.type => }
	}
}
