package t1

trait Element[T] {
}

trait Config {
  type T <: Element[T]
  implicit val m: ClassManifest[T]
  // XXX Following works fine:
  // type T <: Element[_]
}

trait Transform { self: Config =>
  def processBlock(block: Array[T]): Unit = {
    var X = new Array[T](1)
  }
}
