import scala.reflect.runtime.universe._

object Test extends App {
  def m() {
    object Bash {
      val tpe1 = typeOf[Option[_]]
      val tpe2 = typeOf[Option[_]]
      println(tpe1 =:= tpe2)
    }

    Bash
  }

  m()
}