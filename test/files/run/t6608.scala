import reflect.runtime.universe

class C {
  private val yyy: Any = 1
  @inline def foo = yyy
}

object Test extends App {
  import universe._
  val access = typeOf[C].decls
    .toList
    .filter(_.name.toString.endsWith("yyy"))
    .map(x => (x.name, x.isPrivate))
  println(access.head)
}

