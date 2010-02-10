class C {
  @Annot(optionType=classOf[String]) val k = 0
}
object Test {
  def main(args: Array[String]) {
    println(classOf[C].getDeclaredFields.toList.sortBy(f => f.getName).map(f => f.getAnnotations.toList))
  }
}
