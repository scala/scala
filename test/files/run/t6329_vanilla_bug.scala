import scala.reflect.runtime.universe._
import scala.reflect.runtime._
import scala.reflect.ClassManifest

object Test extends App {
  println(implicitly[ClassManifest[List[_]]])
  println(scala.reflect.classTag[List[_]])
}
