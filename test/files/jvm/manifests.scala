object Test extends App {
  Test1
  Test2
  //Test3  // Java 1.5+ only
}

class Foo[T](x: T)
trait Bar[T] { def f: T }

object Test1 extends TestUtil {
  print(())
  print(true)
  print('a')
  print(1)
  print("abc")
  print('abc)
  println()

  print(List(()))
  print(List(true))
  print(List(1))
  print(List("abc"))
  print(List('abc))
  println()

  //print(Array(()))  //Illegal class name "[V" in class file Test$
  print(Array(true))
  print(Array('a'))
  print(Array(1))
  print(Array("abc"))
  print(Array('abc))
  println()

  print(((), ()))
  print((true, false))
  print((1, 2))
  print(("abc", "xyz"))
  print(('abc, 'xyz))
  println()

  // Disabled: should these work? changing the inference for objects from
  // "object Test" to "Test.type" drags in a singleton manifest which for
  // some reason leads to serialization failure.
  // print(Test)
  // print(List)
  println()

  print(new Foo(2))
  print(new Foo(List(2)))
  print(new Foo(new Foo(2)))
  print(new Foo(List(new Foo(2))))
  println()

  print(new Bar[String] { def f = "abc" })
  println()
}

object Test2 {
  import scala.util.Marshal._
  println("()="+load[Unit](dump(())))
  println("true="+load[Boolean](dump(true)))
  println("a="+load[Char](dump('a')))
  println("1="+load[Int](dump(1)))
  println("'abc="+load[Symbol](dump('abc)))
  println()

  println("List(())="+load[List[Unit]](dump(List(()))))
  println("List(true)="+load[List[Boolean]](dump(List(true))))
  println("List('abc)="+load[List[Symbol]](dump(List('abc))))
  println()

  def loadArray[T](x: Array[Byte])(implicit m: reflect.Manifest[Array[T]]) =
    load[Array[T]](x)(m).deepToString
  println("Array()="+loadArray[Int](dump(Array(): Array[Int])))
  println("Array(true)="+loadArray[Boolean](dump(Array(true))))
  println("Array(a)="+loadArray[Char](dump(Array('a'))))
  println("Array(1)="+loadArray[Int](dump(Array(1))))
  println()

  println("((),())="+load[(Unit, Unit)](dump(((), ()))))
  println("(true,false)="+load[(Boolean, Boolean)](dump((true, false))))
  println()

  println("List(List(1), List(2))="+load[List[List[Int]]](dump(List(List(1), List(2)))))
  println()

  println("Array(Array(1), Array(2))="+loadArray[Array[Int]](dump(Array(Array(1), Array(2)))))
  println()
}

object Test3 extends TestUtil {
  import scala.reflect.Manifest._
  val ct1 = classType(classOf[Char])
  val ct2 = classType(classOf[List[_]], ct1)
  print(ct1)
  //print(ct2)  // ??? x=scala.List[char], m=scala.reflect.Manifest[scala.runtime.Nothing$]
  println()
}

trait TestUtil {
  import java.io._
  def write[A](o: A): Array[Byte] = {
    val ba = new ByteArrayOutputStream(512)
    val out = new ObjectOutputStream(ba)
    out.writeObject(o)
    out.close()
    ba.toByteArray()
  }
  def read[A](buffer: Array[Byte]): A = {
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer))
    in.readObject().asInstanceOf[A]
  }
  import scala.reflect._
  def print[T](x: T)(implicit m: Manifest[T]) {
    val m1: Manifest[T] = read(write(m))
    val x1 = x.toString.replaceAll("@[0-9a-z]+$", "")
    println("x="+x1+", m="+m1)
  }
}
