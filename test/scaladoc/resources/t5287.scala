trait t5287_A {
	def method(implicit a: Int): Int = a
}

trait t5287_B extends t5287_A {
	override def method(implicit a: Int): Int = a + 1
}

trait t5287 extends t5287_B{
	/**
	 * Some explanation
	 * 
	 * @usecase def method(): Int
	 * The usecase explanation 
	 */
	override def method(implicit a: Int): Int = a + 3
}