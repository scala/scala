package test

object Test {
  import scala.reflect.runtime.{ universe => ru }
  def getTypeTag(implicit tag: ru.TypeTag[Int]  ) = ()
  locally {
    getTypeTag/*?*/
  }
}
