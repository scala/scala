import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def test[T: ClassTag](x: T) {
    println(classTag[T].runtimeClass.isAssignableFrom(x.getClass))
    println(classTag[T].unapply(x))
  }

  {
    val x = 1.toByte
    println(ClassTag.Byte.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Byte.unapply(x))
    test(x)
  }

  {
    val x = 1.toShort
    println(ClassTag.Short.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Short.unapply(x))
    test(x)
  }

  {
    val x = 1.toChar
    println(ClassTag.Char.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Char.unapply(x))
    test(x)
  }

  {
    val x = 1.toInt
    println(ClassTag.Int.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Int.unapply(x))
    test(x)
  }

  {
    val x = 1.toLong
    println(ClassTag.Long.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Long.unapply(x))
    test(x)
  }

  {
    val x = 1.toFloat
    println(ClassTag.Float.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Float.unapply(x))
    test(x)
  }

  {
    val x = 1.toDouble
    println(ClassTag.Double.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Double.unapply(x))
    test(x)
  }

  {
    val x = true
    println(ClassTag.Boolean.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Boolean.unapply(x))
    test(x)
  }

  {
    val x = ()
    println(ClassTag.Unit.runtimeClass.isAssignableFrom(x.getClass))
    println(ClassTag.Unit.unapply(x))
    test(x)
  }
}