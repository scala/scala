import scala.reflect.runtime.universe._

object Test extends App {
  reify(manifest[Some[_]])
}