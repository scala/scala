
// Was:
// warning: an unexpected type representation reached the compiler backend
// Now:
// error: too many arguments (2) for method apply: (key: Int)scala.collection.mutable.ArrayBuffer[String] in trait MapLike

trait Test {
  import collection.mutable.{Map=>MMap, ArrayBuffer=>AB}

  val m = MMap((1 -> AB("one")))

  val empty = AB[String]()

  m(1, (_ => empty)) ++= AB("eins", "uno")
}

