trait T[A] {
  class X[A] {
    object U {
      object V
    }
  }
}


object Test {
  def main(args: Array[String]): Unit = {
    val U = Class.forName("T$X$U$")
    val V = Class.forName("T$X$U$V$")

    // Was: java.lang.TypeNotPresentException: Type T$X$U$$V$ not present
    assert(V.getEnclosingClass == U, V.getEnclosingClass)
    val tp = U.getMethod("V").getGenericReturnType.asInstanceOf[java.lang.reflect.ParameterizedType]
    val cls = Class.forName(tp.getRawType.getTypeName)
    assert(cls == V, cls)
  }
}

