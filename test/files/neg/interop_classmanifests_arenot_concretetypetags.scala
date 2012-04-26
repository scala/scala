object Test extends App {
  def classManifestIsnotConcreteTypeTag[T: ClassManifest] = {
    println(concreteTypeTag[T])
  }

  classManifestIsnotConcreteTypeTag[Int]
  classManifestIsnotConcreteTypeTag[String]
  classManifestIsnotConcreteTypeTag[Array[Int]]
}