object Test {
  def returnType[T: Manifest](methodName: String) = (
    classManifest[T].erasure.getMethods
    . filter (x => !x.isBridge && x.getName == methodName)
    . map (_.getGenericReturnType.toString)
  )
  def show[T: Manifest](methodName: String) =
    println(manifest[T].erasure.getName +: returnType[T](methodName).distinct mkString " ")

  def main(args: Array[String]): Unit = {
    show[List[_]]("apply")
    show[Option[_]]("get")
    show[Function1[_, _]]("apply")
    show[Traversable[_]]("flatMap")
  }
}
