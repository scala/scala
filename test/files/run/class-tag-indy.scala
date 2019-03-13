// scalac: -Yclass-tag-invoke-dynamic
//
import reflect.ClassTag

object Test {
  def main(args: Array[String]): Unit = {
    // check that the invokedynamic bootstrap caches the classtag 
    // from the first invocation of `cache`.
    assert(cache eq cache)
    assert(dontCache ne dontCache)
  }

  def cache = implicitly[ClassTag[SomeClass]]

  def dontCache[A: ClassTag] = implicitly[ClassTag[Array[A]]]
}

class SomeClass
