import scala.reflect.{ClassTag, classTag}

@deprecated("Suppress warnings", since="2.11")
object Test extends App {
  def testValueClass(tag: ClassTag[_]) {
    println(s"runtimeClass = ${tag.runtimeClass}, toString = ${tag.toString}")
    println(tag <:< tag)
    println(tag <:< ClassTag.AnyVal)
    println(tag <:< ClassTag.Any)
    println(tag <:< ClassTag.Nothing)
    println(ClassTag.Nothing <:< tag)
    println(tag <:< ClassTag.Null)
    println(ClassTag.Null <:< tag)
    println(tag <:< ClassTag.Object)
    println(ClassTag.Object <:< tag)
  }

  testValueClass(ClassTag.Byte)
  testValueClass(ClassTag.Short)
  testValueClass(ClassTag.Char)
  testValueClass(ClassTag.Int)
  testValueClass(ClassTag.Long)
  testValueClass(ClassTag.Float)
  testValueClass(ClassTag.Double)
  testValueClass(ClassTag.Unit)
  testValueClass(ClassTag.Boolean)
}
