@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  import scala.reflect.ClassManifest
  println(implicitly[ClassManifest[List[_]]])
  println(scala.reflect.classTag[List[_]])
}
