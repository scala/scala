class A[+U >: Null] {
  type R[+X >: Null] = X
  type O[+X] = A[R[X]]
}

// with the following error:
// 
// type arguments [A.this.R[X]] do not conform to class A's type parameter bounds [+U >: Null]
// 
// However, because type R[+X>:Null] is identical to X, it should carry X bounds and R[X] lower bound should be known to be X's lower bound, i.e. Null.
// 
// The same problem occurs with upper bounds.
