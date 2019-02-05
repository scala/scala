import language.dynamics

case class ArtifactGroup(org: String, pre: String, rev: String) extends Dynamic {
  def selectDynamic(name: String) = s"$org:$pre-$name:$rev"
}

object Test {
  val library = ArtifactGroup("org.scala", "amazing-library", "7.2.4")

  def a = Seq(library.core, library.mail)
  def b = Seq(a: _*)
  def c = Seq(Seq(library.core, library.mail): _*)
}
