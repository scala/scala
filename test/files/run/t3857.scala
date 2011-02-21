class ScalaGeneric { var s: java.util.Set[String] = _ }

trait ScalaGeneric2Trait { var s: java.util.Set[String] = _ }
class ScalaGeneric2 extends ScalaGeneric2Trait

object Test extends App {
  println(classOf[ScalaGeneric].getDeclaredField("s").toGenericString)
  // java.util.Set<java.lang.String> ScalaGeneric.s

  println(classOf[ScalaGeneric2].getDeclaredField("s").toGenericString)
  // java.util.Set ScalaGeneric2.s -- no signature should be found because it was mixed in.
}
