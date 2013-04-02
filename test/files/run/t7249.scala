object Test extends App {
  def bnToLambda(s: => String): () => String = () => s
  var x: () => String = () => sys.error("Nope")
  val y = bnToLambda { x() }
  x = () => "Yup!"
  println(y())
}
