trait SI_5287_A {
	def method(implicit a: Int): Int = a
}

trait SI_5287_B extends SI_5287_A {
	override def method(implicit a: Int): Int = a + 1
}

trait SI_5287 extends SI_5287_B{
	/**
	 * Some explanation
	 * 
	 * @usecase def method(): Int
	 * The usecase explanation 
	 */
	override def method(implicit a: Int): Int = a + 3
}