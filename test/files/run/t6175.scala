object Test extends App {
  import reflect.runtime._
  val m = universe.typeOf[List[_]].members.head.asMethod
  currentMirror.reflect (List (2, 3, 1)).reflectMethod(m)
}