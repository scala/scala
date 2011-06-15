class TypeAliasVsImplicitTest {

    class For[m[_], a](x: m[a]) {
       def map[b](y: a => b): m[b] = throw new Error
    }
    implicit def toFor[m[_], a](x: m[a]): For[m, a] = new For[m, a](x)

    trait MyList[A]

    def foo(xs: MyList[Int]) = xs.map(x => x) // compiles fine.

    type MyListOfInt = MyList[Int]
    def bar(xs: MyListOfInt) = xs.map(x => x) // doesn't compile: value map is not a member of TypeAliasVsImplicitTest.this.MyListOfInt
}

// minimal case -- the bug was in type constructor inference where `xs.type` needed to be widened *and* dealiased
// in 2.8.1 implicit conversion search started with a widened type, so that combo never came up
// object Test {
//   class For[m[_], a](x: m[a])
//   def toFor[m[_], a](x: m[a]): For[m, a] = new For[m, a](x)
//
//   trait MyList[A]
//   type MyListOfInt = MyList[Int]
//
//   val xs: MyListOfInt = error("")
//   toFor(xs : xs.type)
// }