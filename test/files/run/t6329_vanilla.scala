import scala.reflect.{ClassManifest, classTag}

@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  println(implicitly[ClassManifest[scala.List[_]]])
  println(classTag[scala.List[_]])
  println(implicitly[ClassManifest[scala.collection.immutable.List[_]]])
  println(classTag[scala.collection.immutable.List[_]])
  println(implicitly[ClassManifest[Predef.Set[_]]])
  println(classTag[Predef.Set[_]])
  println(implicitly[ClassManifest[scala.collection.immutable.Set[_]]])
  println(classTag[scala.collection.immutable.Set[_]])
}
