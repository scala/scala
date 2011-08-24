object T1 {
  trait T[A]
  class C extends T[String]
  object Test {
    def main(args: Array[String]): Unit = {
      classOf[C].getTypeParameters
    }
  }
}

object T2 {
  trait T[A]
  class C extends T[String]
  object Test {
    def main(args: Array[String]): Unit = {
      val x = classOf[C]
      x.getTypeParameters
    }
  }
}

object T3 {
  trait T[A]
  class C extends T[String]
  object Test {
    def main(args: Array[String]): Unit = {
      val x: Class[C] = classOf[C]
      x.getTypeParameters
    }
  }
}