package p1 {
  package p2 {
    object Singleton {
      object Singleton {
        object Singleton
      }
    }
  }
}


object Test extends App {
  import p1.p2._
  println(Singleton.Singleton.getClass)
  println(Singleton.Singleton.getClass.getDeclaredClasses.toList)
}
