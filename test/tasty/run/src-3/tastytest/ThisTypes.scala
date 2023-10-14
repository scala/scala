package tastytest

object ThisTypes {

  abstract class Wrap[T] {
    type Base[A] <: { // if not resolved to Sub.this.Base then reflective calls will be needed
      def x: A
    }
    final type Res = Option[Base[T]]
    def doTest: Res
  }

  class Sub extends Wrap[Int] {
    class BaseImpl[A](a: A) {
      def x: A = a
    }
    override type Base[A] = BaseImpl[A]
    def doTest: Res = Some(new BaseImpl(23))
  }

  abstract class Wrap2[T] {
    class Base[A](a: A) {
      def x: A = a
    }
    final type Res = Option[Base[T]]
    def doTest: Res
  }

  class Sub2 extends Wrap2[Int] {
    def doTest: Res = Some(new Base(23))
  }

}
