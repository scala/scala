// test well-kindedness checks

// expecting types of kind *->*
class Monad[m[x]]
trait ms1 extends Monad[String]        // wrong
trait ms2[t] extends Monad[t]          // wrong
trait ms3[m[_], t] extends Monad[m[t]] // wrong -- added to check regression on bug

// expecting types of kind *
trait Foo[x]
trait Bar1[m[_]] extends Foo[m[Int]] // check that m[Int] is properly recognized as kind-*
trait Bar2[m[_]] extends Foo[m] // check that m is properly recognized as kind *->*, while * is expected
