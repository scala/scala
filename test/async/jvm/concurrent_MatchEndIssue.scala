//> using options -Xasync

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.tools.testkit.async.Async._
import scala.concurrent.duration.Duration

sealed trait Subject

final class Principal(val name: String) extends Subject

object Principal {
  def unapply(p: Principal): Option[String] = Some(p.name)
}

object Test extends App { test
  def containsPrincipal(search: String, value: Subject): Future[Boolean] = async {
    value match {
      case Principal(name) if name == search => true
      case Principal(name) => await(containsPrincipal(search, value))
      case other => false
    }
  }

  def test = Await.result(containsPrincipal("test", new Principal("test")), Duration.Inf)
}
