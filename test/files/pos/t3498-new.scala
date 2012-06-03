import scala.reflect.{ArrayTag, arrayTag}

abstract class A[T, @specialized(scala.Int) U : ArrayTag] {
    def f(state: T): Array[U]
}

abstract class B extends A[ Array[Byte], Int ] {
    type T = Array[Byte]
    type U = Int

    val N = 0

    def f(state: T): Array[U] =
    {
        new Array[U](N + state(N))
    }
}