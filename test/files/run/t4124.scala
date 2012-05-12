import xml.Node

object Test extends App {
  val body: Node = <elem>hi</elem>	
  println ((body: AnyRef, "foo") match {
    case (node: Node, "bar")        => "bye"
    case (ser: Serializable, "foo") => "hi"
  })

  println ((body, "foo") match {
    case (node: Node, "bar")        => "bye"
    case (ser: Serializable, "foo") => "hi"
  })

  println ((body: AnyRef, "foo") match {
    case (node: Node, "foo")        => "bye"
    case (ser: Serializable, "foo") => "hi"
  })

  println ((body: AnyRef, "foo") match {
    case (node: Node, "foo")        => "bye"
    case (ser: Serializable, "foo") => "hi"
  })
}
