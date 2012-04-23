object Test extends App {
  def concreteTypeTagIsClassManifest[T: ConcreteTypeTag] = {
    println(classManifest[T])
  }

  concreteTypeTagIsClassManifest[Int]
  concreteTypeTagIsClassManifest[String]
  concreteTypeTagIsClassManifest[Array[Int]]
}