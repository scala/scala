import scala.annotation.nolink

abstract class A[T] {
  def f: T
  def g(t: T): Int
}

class BI extends A[Int] {
  @nolink
  def f: Int = 11

  @nolink
  def g(i: Int): Int = i + 11
}

class BS extends A[String] {
  @nolink
  def f: String = " hai "

  @nolink
  def g(s: String): Int = s.trim.length
}

class BIO1 extends BI {
  override def f: Int = super.f + 22

  override def g(i: Int): Int = super.g(i + 22) + 33
}

class BSO1 extends BS {
  override def f: String = super.f.trim()

  override def g(s: String): Int = super.g(s.trim) + 33
}

class BIO2 extends BI {
  @nolink
  override def f: Int = super.f + 22

  @nolink
  override def g(i: Int): Int = super.g(i + 22) + 33
}

class BSO2 extends BS {
  @nolink
  override def f: String = super.f.trim()

  @nolink
  override def g(s: String): Int = super.g(s.trim) + 33
}

trait T0 {
  def meth: Object = "T0"
}

trait T1 extends T0 {
  @nolink override def meth: String = "T1"
}

class C extends T1

object Test {
  def show(i: Int) = println(i)
  def show(s: String) = println(s)
  def main(args: Array[String]): Unit = {
    val bi = new BI
    show(bi.f + 11)
    show(bi.g(11))

    val bs = new BS
    show(bs.f.trim)
    show(bs.g(" hui "))

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

    println((new C).meth)
  }
}
