object Test extends App {
  def classManifestIspartiallyTypeTag[T: ClassManifest] = {
    println(typeTag[T].tpe)
    println(typeTag[T].erasure)
  }

  classManifestIspartiallyTypeTag[Int]
  classManifestIspartiallyTypeTag[String]
  classManifestIspartiallyTypeTag[Array[Int]]
}