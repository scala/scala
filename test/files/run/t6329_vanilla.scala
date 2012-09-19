object Test extends App {
  println(classManifest[List[_]])
  println(scala.reflect.classTag[List[_]])
}