class C {
  // this always worked
  // during isApplicableToMethod will use formalTypes to eliminate the repeated param in the formal types,
  // but we keep the repeated marker in the arguments -- here's a debug log:
/*
isCompatibleArgs false (List(Int*), List(Int))
isAsSpecific false: (xs: Int*)Int >> (x: Int)Int?
 --> the repeated case is not more specific than the single-arg case because
     you can't apply something of `Int*` to `Int`

isCompatibleArgs true (List(Int), List(Int))
isAsSpecific true: (x: Int)Int >> (xs: Int*)Int?
  --> the single param case is more specific than the repeated param case, because
      you can apply a single argument to the method with the repeated param

isCompatibleArgs true (List(Int), List(Int))
isAsSpecific true: (x: Int)Int >> (xs: Int*)Int?
isCompatibleArgs false (List(Int*), List(Int))
isAsSpecific false: (xs: Int*)Int >> (x: Int)Int?
isCompatibleArgs true (List(Int), List(Int))
isAsSpecific true: (x: Int)Int >> (xs: Int*)Int?
isCompatibleArgs false (List(Int*), List(Int))
isAsSpecific false: (xs: Int*)Int >> (x: Int)Int?
inferMethodAlternative applicable List(method foo, method foo) --> ranked: List(method foo)

 */

  def foo(xs: Int*): Int = xs.toSeq.head
  def foo(x: Int): Int = x
  foo(2)

  // this should also type check, resolving to the non-repeated case,
  // but there was a bug in the polymorphic case of isApplicableToMethod
  // (adjustTypeArgs would remove the incompatibility in applying something
  //  expecting type T to a T*, as the latter would be turned into Seq[T])
/*
isAsSpecific false: [T](xs: T*)T >> [T](x: T)T?
isAsSpecific true: [T](x: T)T >> [T](xs: T*)T?
isAsSpecific true: [T](x: T)T >> [T](xs: T*)T?
isAsSpecific false: [T](xs: T*)T >> [T](x: T)T?
isAsSpecific true: [T](x: T)T >> [T](xs: T*)T?
isAsSpecific false: [T](xs: T*)T >> [T](x: T)T?
inferMethodAlternative applicable List(method fooT, method fooT) --> ranked: List(method fooT)
 */
  def fooT[T](xs: T*): T = xs.toSeq.head
  def fooT[T](x: T): T = x
  fooT(2)

  // from 4775
  def f[T](x: T): T = x
  def f[T](x: T, xs: T*): T = x

  f(5)
}
