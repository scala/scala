import reflect.ClassTag

object Test {
  def main(args: Array[String]): Unit = {
    assert(implicitly[ClassTag[SomeClass]] eq implicitly[ClassTag[SomeClass]])
    assert(implicitly[ClassTag[Array[SomeClass]]] eq implicitly[ClassTag[Array[SomeClass]]])
  }
}

class SomeClass
