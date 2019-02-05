import scala.reflect.runtime.universe._

object Bosh {
  def Besh: Unit = {
    new {
      val t = typeOf[Option[_]]
      val x = t
    }
  }
}
