class A2 extends A1 with T1{
	def f2 = { def g = 5 ; class A { def a = g }; g ; new A().a }
}

object Test extends A2 {
	def main(args: Array[String]) {
    println(Seq(classOf[T1], classOf[A1], classOf[A2]).flatMap(_.getDeclaredMethods.map(_.toString).sorted).mkString("\n"))
  }
}
