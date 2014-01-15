class HListBench {

  class A[H, T]

  type B[H, T] = A[H, T]

  // was okay
  type T1 = A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, A[Int, Nothing]]]]]]]]]]]]]]]]]]]]]]]]]]]]

  // Took over a minute to validate variance in 2.10.3!
  type T2 = B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, B[Int, Nothing]]]]]]]]]]]]]]]]]]]]]]]]]]]]

}