object Test {
  def f1(clazz: Class[_]) = (
    clazz.getDeclaredFields.toList
     . filterNot (_.getName contains "bitmap$")
     . map (f => (f.getName, f.getGenericType))
     . foreach (println)
  )
  def f2(clazz: Class[_]) = (
    clazz.getDeclaredMethods.toList
     . filterNot (_.getName contains "bitmap$")
     . map (f => (f.getName, f.getGenericReturnType))
     . foreach (println)
  )

  def main(args: Array[String]): Unit = {
    f1(classOf[One])
    f2(classOf[One])
    f1(classOf[Two])
    f2(classOf[Two])

    new J_2().javaRun
  }
}
