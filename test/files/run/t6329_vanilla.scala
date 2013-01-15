import scala.reflect.classTag

object Test extends App {
  println(classManifest[scala.List[_]])
  println(classTag[scala.List[_]])
  println(classManifest[scala.collection.immutable.List[_]])
  println(classTag[scala.collection.immutable.List[_]])
  println(classManifest[Predef.Set[_]])
  println(classTag[Predef.Set[_]])
  println(classManifest[scala.collection.immutable.Set[_]])
  println(classTag[scala.collection.immutable.Set[_]])
}
