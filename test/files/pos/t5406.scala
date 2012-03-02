object Wuffles { }
object Test {
  def f = (Some(Wuffles): Option[Wuffles.type]) match { case Some(Wuffles) => println("Woof"); case _ => println("Meow") }
}
