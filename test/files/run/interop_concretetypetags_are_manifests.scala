object Test extends App {
  def concreteTypeTagIsManifest[T: ConcreteTypeTag] = {
    println(manifest[T])
  }

  concreteTypeTagIsManifest[Int]
  concreteTypeTagIsManifest[String]
  concreteTypeTagIsManifest[Array[Int]]
}