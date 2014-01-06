object Test extends App {
  import scala.reflect.runtime.universe._
  def Name[T:TypeTag](name:String): T = implicitly[TypeTag[T]] match {
    case t => newTypeName(name).asInstanceOf[T]
  }
  val X = "ASDF"
  println(q"class ${Name(X)} { }")
}
