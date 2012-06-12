object Test {
  trait NT[X]
  trait W[W, A] extends NT[Int]
  type StringW[T] = W[String, T]
  trait K[M[_], A, B]

  def k[M[_], B](f: Int => M[B]): K[M, Int, B] = null

  val okay1: K[StringW,Int,Int] = k{ (y: Int) => null: StringW[Int] }
  val okay2 = k[StringW,Int]{ (y: Int) => null: W[String, Int] }

  val crash: K[StringW,Int,Int] = k{ (y: Int) => null: W[String, Int] }

  // remove `extends NT[Int]`, and the last line gives an inference error
  // rather than a crash.
  //   test/files/pos/t5683.scala:12: error: no type parameters for method k: (f: Int => M[B])Test.K[M,Int,B] exist so that it can be applied to arguments (Int => Test.W[String,Int])
  //  --- because ---
  // argument expression's type is not compatible with formal parameter type;
  //  found   : Int => Test.W[String,Int]
  //  required: Int => ?M[?B]
  //   val crash: K[StringW,Int,Int] = k{ (y: Int) => null: W[String, Int] }
  //                                   ^
}
