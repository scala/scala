import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def test[T: ClassTag](x: T) {
    println(s"Checking if ${x.getClass} matches ${classTag[T].runtimeClass}")
    println(classTag[T].unapply(x))
  }

  {
    val x = 1.toByte
    println(s"Checking if ${x.getClass} matches ${classTag[Byte].runtimeClass}")
    println(ClassTag.Byte.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Short].runtimeClass}")
    println(ClassTag.Short.unapply(x))
    test(x)
  }

  {
    val x = 1.toShort
    println(s"Checking if ${x.getClass} matches ${classTag[Short].runtimeClass}")
    println(ClassTag.Short.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Char].runtimeClass}")
    println(ClassTag.Char.unapply(x))
    test(x)
  }

  {
    val x = 1.toChar
    println(s"Checking if ${x.getClass} matches ${classTag[Char].runtimeClass}")
    println(ClassTag.Char.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Int].runtimeClass}")
    println(ClassTag.Int.unapply(x))
    test(x)
  }

  {
    val x = 1.toInt
    println(s"Checking if ${x.getClass} matches ${classTag[Int].runtimeClass}")
    println(ClassTag.Int.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Long].runtimeClass}")
    println(ClassTag.Long.unapply(x))
    test(x)
  }

  {
    val x = 1.toLong
    println(s"Checking if ${x.getClass} matches ${classTag[Long].runtimeClass}")
    println(ClassTag.Long.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Float].runtimeClass}")
    println(ClassTag.Float.unapply(x))
    test(x)
  }

  {
    val x = 1.toFloat
    println(s"Checking if ${x.getClass} matches ${classTag[Float].runtimeClass}")
    println(ClassTag.Float.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Double].runtimeClass}")
    println(ClassTag.Double.unapply(x))
    test(x)
  }

  {
    val x = 1.toDouble
    println(s"Checking if ${x.getClass} matches ${classTag[Double].runtimeClass}")
    println(ClassTag.Double.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Boolean].runtimeClass}")
    println(ClassTag.Boolean.unapply(x))
    test(x)
  }

  {
    val x = true
    println(s"Checking if ${x.getClass} matches ${classTag[Boolean].runtimeClass}")
    println(ClassTag.Boolean.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Unit].runtimeClass}")
    println(ClassTag.Unit.unapply(x))
    test(x)
  }

  {
    val x = ()
    println(s"Checking if ${x.getClass} matches ${classTag[Unit].runtimeClass}")
    println(ClassTag.Unit.unapply(x))
    println(s"Checking if ${x.getClass} matches ${classTag[Byte].runtimeClass}")
    println(ClassTag.Byte.unapply(x))
    test(x)
  }
}
