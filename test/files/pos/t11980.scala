//scalac: -Werror -Wunused:privates -Xsource:3
//
object Domain {
  def id(id: String): Domain = Domain(Some(id), None)
  def name(name: String): Domain = Domain(None, Some(name))

  // induces private copy and private copy defaults
  def apply(id: String, name: String): Domain = Domain(Some(id), Some(name))
}

case class Domain private (id: Option[String], name: Option[String])

// t7707
object O { O() ; def f(): Unit = O() }
case class O private (x: Int = 3)
