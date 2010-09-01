object Test {
 trait A[X] { type A[x <: X] = x }
 val a = (new A[String]{}): { type A[x <: String] } // ok
 val b = (new A[String]{}): { type A[x] } // not ok
 val c = (new A[String]{}): { type A } // not ok

 val x = (new { type A = String }): { type A[X] } // not ok
//a: AnyRef{type A[X]}

  identity[x.A[Any]] _
}