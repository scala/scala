package tastytest

object AppliedTypeLambda:

  // This reflects the state of context function expansion in Scala 3.5.0-RC1
  // this was fixed in 3.5.0-RC3, but technically any user could type this?
  def emptyArray[T](using ct: ([T1] =>> reflect.ClassTag[T1])[T]): Array[T] =
    new Array[T](0)

  def emptyArray2[T, F[T2] <: ([T1] =>> reflect.ClassTag[T1])[T2]](using ct: F[T]): Array[T] =
    new Array[T](0)

  class InTypeParam[F[T] <: ([T1] =>> reflect.ClassTag[T1])[T]]:
    def emptyArray[T](using ct: F[T]): Array[T] = new Array[T](0)

