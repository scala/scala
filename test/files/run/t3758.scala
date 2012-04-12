object Test {
  def main(args: Array[String]): Unit = {
    println(classManifest[Array[String]].tpe.typeArguments)
    println(classManifest[Array[Int]].tpe.typeArguments)
    println(classManifest[Array[Float]].tpe.typeArguments)
    println(manifest[Array[String]].tpe.typeArguments)
    println(manifest[Array[Int]].tpe.typeArguments)
    println(manifest[Array[Float]].tpe.typeArguments)
  }
}
