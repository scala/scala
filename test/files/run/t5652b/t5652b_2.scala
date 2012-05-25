class A2 extends A1 {
	def f2 = { def g = 5 ; class A { def a = 0 } ; new A; g }
}

object Test extends A2 {
  def main(args: Array[String]) {
    println(Seq(classOf[A1], classOf[A2]).flatMap(_.getDeclaredMethods.map(_.toString).sorted).mkString("\n"))
  }
}
