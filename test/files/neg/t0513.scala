object Test {
    case class Y[T1, T2 <: T1]()
    //val test = Y[Nothing, Int]  // Compiler error
    case class Test[T]()
    val test2 = Test[Y[Nothing, Int]]  // No error
}
