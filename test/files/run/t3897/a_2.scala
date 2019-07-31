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
    println("one fields")
    f1(classOf[One])
    println("one methods")
    f2(classOf[One])
    println("two fields")
    f1(classOf[Two])
    println("two methods")
    f2(classOf[Two])

    println("now java")
    new J_2().javaRun
  }
}
