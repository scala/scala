import scala.reflect.runtime.universe._

object Test extends App {
  println(rootMirror.runtimeClass(typeOf[Array[Unit]]))
}