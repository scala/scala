abstract class A[T] {
  def f: T
  def g(t: T): Int
}

import scala.annotation.inheritSignature

class BI extends A[Int] {
  @inheritSignature
  def f: Int = 11 // need box - return type in bytecode is Object

  @inheritSignature
  def g(i: Int): Int = i + 11 // need unbox - param type in bytecode is Object
}

class BS extends A[String] {
  @inheritSignature
  def f: String = " hai " // ok - return type in bytecode is Object

  @inheritSignature
  def g(s: String): Int = s.trim.length // need cast - param type in bytecode is Object
}

class BIO1 extends BI {
  // works fine. generates a bridge.
  // genearates public `f: Int` has the `override` flag, but that's fine as it doesn't exist in bytecode
  override def f: Int = super.f + 22

  override def g(i: Int): Int = super.g(i + 22) + 33
}

class BSO1 extends BS {
  override def f: String = super.f.trim()

  override def g(s: String): Int = super.g(s.trim) + 33
}

class BIO2 extends BI {
  // works fine. bridge becomes override, `f: Int` stays private
  @inheritSignature
  override def f: Int = super.f + 22

  @inheritSignature
  override def g(i: Int): Int = super.g(i + 22) + 33
}

class BSO2 extends BS {
  @inheritSignature
  override def f: String = super.f.trim()

  @inheritSignature
  override def g(s: String): Int = super.g(s.trim) + 33
}

object Test {
  def show(i: Int) = println(i)
  def show(s: String) = println(s)
  def showSigs[T](implicit t: reflect.ClassTag[T]) = {
    val c = t.runtimeClass
    println(c.getName)
    println(c.getDeclaredMethods.filter(m => m.getName == "f" || m.getName == "g").map(_.toString).sorted.mkString("- ", "\n- ", ""))
  }
  def main(args: Array[String]): Unit = {
    val bi = new BI
    show(bi.f + 11)  // need unbox
    show(bi.g(11))   // need box

    val bs = new BS
    show(bs.f.trim)      // need cast
    show(bs.g(" hui "))  // ok

    val bio1 = new BIO1
    show(bio1.f + 11)
    show(bio1.g(11))

    val bso1 = new BSO1
    show(bso1.f.trim)
    show(bso1.g(" hui "))

    val bio2 = new BIO2
    show(bio2.f + 11)
    show(bio2.g(11))

    val bso2 = new BSO2
    show(bso2.f.trim)
    show(bso2.g(" hui "))

    showSigs[A[_]]
    showSigs[BI]
    showSigs[BS]
    showSigs[BIO1]
    showSigs[BSO1]
    showSigs[BIO2]
    showSigs[BSO2]
  }
}
