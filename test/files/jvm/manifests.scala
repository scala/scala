object Test extends Application {
  import scala.reflect._

  def manifestOf[T](implicit m: Manifest[T]): Manifest[T] = m

  def print[T](x: T)(implicit m: Manifest[T]) {
    val m1: Manifest[T] = Serialize.read(Serialize.write(m))
    val x1 = x.toString.replaceAll("@[0-9a-z]+$", "")
    println("x="+x1+", m="+m1)
  }
  print(())
  print(true)
  print('a')
  print(1)
  print("abc")

  print(List(()))
  print(List(true))
  print(List(1))
  print(List("abc"))

  //print(Array(()))  //Illegal class name "[V" in class file Test$
  print(Array(true))
  print(Array('a'))
  print(Array(1))
  print(Array("abc"))

  print(((), ()))
  print((true, false))
  print((1, 2))
  print(("abc", "xyz"))

  print(Serialize)
  print(Test)
  print(List)

  class Foo[T](x: T)
  print(new Foo(2))
  print(new Foo(List(2)))
  print(new Foo(new Foo(2)))
  print(new Foo(List(new Foo(2))))

  trait Bar[T] { def f: T }
  print(new Bar[String] { def f = "abc" })
}

object Serialize {
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
}

