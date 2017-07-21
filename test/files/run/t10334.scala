import scala.language.reflectiveCalls

object Test {
  def main(args: Array[String]): Unit = {
    assert(t1 == "hi")
    assert(t2 == 1)
    t3()
  }

  def t1: Object = {
    val f: { def apply(s: String): Object } = (x: String) => x
    f("hi")
  }

  def t2: Int = {
    def byName(b: => Int): Int = b
    def namer[A, B](f: A => B): (A => B) { def apply(i: A): B } = f

    val namedFunction = namer(byName _)
    namedFunction(1)
  }

  // Not sure how to fix this one.. https://github.com/scala/bug/issues/10334
  def t3(): Unit = {
    val f1 = new T[A] {
      def m(x: A) = "f1-a"
      def m(x: B) = "f1-b"
       // the m(Object)Object bridge method invokes (A)Object
    }

    val f2 = new T[B] {
      def m(x: A) = "f2-a"
      def m(x: B) = "f2-b"
       // the (Object)Object bridge method invokes (B)Object
    }

    val g1: T[C] = f1
    val g2: T[C] = f2

    assert(g1.m(new C) == "f1-a")
    assert(g2.m(new C) == "f2-b")

    val s1: { def m(s: C): Object } = g1
    val s2: { def m(s: C): Object } = g2

    // the reflective lookup doesn't find `m(C)Object`
    try {
      s1.m(new C) // should invoke `m(A)Object`
      throw new Error()
    } catch {
      case _: java.lang.NoSuchMethodException =>
    }

    // the reflective lookup doesn't find `m(C)Object`
    try {
      s2.m(new C) // should invoke `m(B)Object`
      throw new Error()
    } catch {
      case _: java.lang.NoSuchMethodException =>
    }
  }
}

class A
class B extends A
class C extends B

trait T[-A] {
  def m(a: A): Object
}
