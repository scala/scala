
import java.util.{List => JList}
import scala.collection.mutable
import scala.collection.JavaConverters.asScalaBuffer
import scala.language.existentials
import scala.language.implicitConversions

object Test extends App {

  def fromJava[T](li: JList[T]): List[T] = asScalaBuffer(li).toList

  implicit def `list asScalaBuffer`[A](l: JList[A]): mutable.Buffer[A] = asScalaBuffer(l)

  // implicit conversion - ok
  def test1(jList:JList[_]) = jList.filter(_.isInstanceOf[java.lang.Object])

  // explicit conversion - ok
  def test2(jList:JList[_]) = {
    val f = fromJava(jList).filter _
    f(_.isInstanceOf[java.lang.Object])
  }

  // implicit conversion - error
  def test3(jList:JList[_]) = {
    val f = jList.filter _
    f(_.isInstanceOf[java.lang.Object])
  }

  val ss: JList[String] = {
    val res = new java.util.ArrayList[String]
    res.add("hello, world")
    res
  }

  println {(
    test1(ss),
    test2(ss),
    test3(ss),
  )}

}
