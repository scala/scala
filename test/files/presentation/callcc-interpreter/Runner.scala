import scala.tools.nsc.interactive.tests._

object Test extends InteractiveTest {
  settings.XoldPatmat.value = true // TODO: could this be running into some kind of race condition? sometimes the match has been translated, sometimes it hasn't
}