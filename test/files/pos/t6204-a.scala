import scala.reflect.runtime.universe._

object Bish {
  def m: Unit = {
    object Bash {
      typeOf[Option[_]]
    }
  }
}
