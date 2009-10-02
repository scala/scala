val a: Vector[_ <: Vector[Any]] = Array(Array("", 0))
val x = a(0) // java.lang.ClassCastException: [Ljava.lang.Object; cannot be cast to scala.collection.mutable.Vector
