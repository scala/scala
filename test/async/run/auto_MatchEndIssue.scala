import scala.tools.nsc.transform.async.user.{async, autoawait}

sealed trait Subject

final class Principal(val name: String) extends Subject

object Principal {
  def unapply(p: Principal): Option[String] = Some(p.name)
}

object Test extends App { test
  @autoawait
  @async
  def containsPrincipal(search: String, value: Subject): Boolean = value match {
    case Principal(name) if name == search => true
    case Principal(name)                   => containsPrincipal(search, value)
    case other                             => false
  }

  @async
  def test = containsPrincipal("test", new Principal("test"))
}
