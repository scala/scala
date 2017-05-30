object Test {
  trait Binary[A, B]

  type Unary[A] = Binary[A, A]

  def f[F[A], A](f: F[A]) = ???
  def test1(u: Unary[Any]) = f(u)

  // reports inference error, cannot unifiy Binary[Any, Any] with ?F[?A]
  // commented out as we can't have errors in a test for warnings.
  // def test2(u: Binary[Any, Any]) = f(u)

  def test3 = {
    implicit def b2u[A, B](b: Binary[A, B]): List[Int] = ???
    val b: Binary[Any, Any] = null
    f(b) // inference fails initially, but then we try to coerse the arguments 
    //
    // Under -Ytyper-debug, we see:
    //
    //    searching for adaptation to pt=Test.Binary[Any,Any] => ?F[?A] (silent: method test3 in Test) implicits disabled
    // |    |    |    |    5 eligible for pt=Test.Binary[Any,Any] => ?F[?A] at (silent: method test3 in Test) implicits disabled
    // |    |    |    |    [search #5] considering b2u
    // |    |    |    |    |-- b2u BYVALmode-EXPRmode-FUNmode-POLYmode (silent: method test3 in Test) implicits disabled
    // |    |    |    |    |    [adapt] [A, B](b: Test.Binary[A,B])List[Int] adapted to [A, B](b: Test.Binary[A,B])List[Int]
    // |    |    |    |    |    \-> (b: Test.Binary[A,B])List[Int]
    // |    |    |    |    solving for (A: ?A, B: ?B)
    // |    |    |    |    [adapt] [A, B](b: Test.Binary[A,B])List[Int] adapted to [A, B](b: Test.Binary[A,B])List[Int] based on pt Test.Binary[Any,Any] => ?F[?A]
    // |    |    |    |    [search #5] success inferred value of type Test.Binary[Any,Any] => ?F[?A] is SearchResult(b2u[Any, Any], )
    //
    // This leads to inference of ?F=Any, ?A=Nothing (this is kind-correct because Any/Nothing are kind polymorphic)
    // 
    // When we instantatiate the method type of `f` with this, the formal parameter type is now just Any[Nothing]
    // which is just Any.
    //
    // The provided argument of type `Binary[A, B]` now unifies with this, no implicit coercion required.
  }

  f[Any, Nothing]("") // explicit type application incurs no warning.
}
