import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.{ClassTag, classTag}

object Test extends App {
  def test[T: ClassTag: TypeTag](x: T) = {
    println(s"========${classTag[T].runtimeClass}========")
    println(cm.reflect(x).reflectMethod(typeOf[T].member(TermName("getClass")).asMethod)())
    println(cm.reflect(x).reflectMethod(typeOf[T].member(TermName("toString")).asMethod)())
  }

  test(2.toByte)
  test(2.toShort)
  test(2.toInt)
  test(2.toLong)
  test(2.toFloat)
  test(2.toDouble)
  test('2')
  test(true)
  test(())
}