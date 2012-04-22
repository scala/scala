object Test extends App {
  def inferredType[T : Manifest](v : T) = println(manifest[T])

  trait A
  trait B

  inferredType(new A with B)

  val name = new A with B
  inferredType(name)
}