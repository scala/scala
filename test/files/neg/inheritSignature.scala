import scala.annotation.inheritSignature

class A { def f: Object = "a" }

class B1 extends A  {                   override def f: String = "b" }
class B2 extends A  { @inheritSignature override def f: String = "b" }

class C1 extends B1 { @inheritSignature override def f: String = "c" } // error, same signature
class C2 extends B2 { @inheritSignature override def f: String = "c" } // ok

class D { @inheritSignature def f: String = "d" } // error, overrides nothing

object VC {
  trait A[T] extends Any {
    def f: Object = "a"
    def g(x: T): String = "a"
  }
  implicit class C(private val x: String) extends AnyVal with A[Int] {
    @annotation.inheritSignature override def f: String = "c" // message is not very accurate, but it's a niche feature anyway
    @annotation.inheritSignature override def g(x: Int): String = "c" // message is not very accurate, but it's a niche feature anyway
  }
}
