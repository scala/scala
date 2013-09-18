
import scala.language.{ postfixOps }
import java.{ lang => jl }

trait T[A] {
  def f(): A
}
class C extends T[Char] {
  def f(): Char = 'a'
}
class Arr {
  def arr1(xs: Array[Int]): List[Int] = xs.toList
  def arr2(xs: Array[jl.Character]): List[jl.Character] = xs.toList
  def arr3(xss: Array[Array[Float]]): Array[Float] = xss map (_.sum)
  // This gets a signature like
  // public <T> java.lang.Object Arr.arr4(java.lang.Object[],scala.reflect.Manifest<T>)
  //
  // instead of the more appealing version from the past
  // public <T> T[] Arr.arr4(T[][],scala.reflect.Manifest<T>)
  //
  // because java inflict's its reference-only generic-arrays on us.
  //
  def arr4[T: Manifest](xss: Array[Array[T]]): Array[T] = xss map (_.head)
}

object Test {
  val c1: Class[_] = classOf[T[_]]
  val c2: Class[_] = classOf[C]
  val c3: Class[_] = classOf[Arr]

  val c1m = c1.getMethods.toList filter (_.getName == "f") map (_.getGenericReturnType.toString)
  val c2m = c2.getMethods.toList filter (_.getName == "f") map (_.getGenericReturnType.toString)
  val c3m = c3.getDeclaredMethods.toList map (_.toGenericString)

  def main(args: Array[String]): Unit = {
    println(c2.getGenericInterfaces.map(_.toString).sorted mkString " ")
    println(c1m ++ c2m sorted)
    println(new C f)
    c3m.sorted foreach println
  }
}
