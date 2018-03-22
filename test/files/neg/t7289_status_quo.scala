// record the status quo after this fix
// not clear to @adriaanm why an upper-bounded existential in an invariant position
// is different from putting that upper bound in a covariant position
object Test1 {
  trait Ext[T]
  implicit def f[A, Coll <: CC[A], CC[X] <: Traversable[X]](implicit xi: Ext[A]): Ext[Coll] = ???
  implicit val m: Ext[List[List[Int]]] = new Ext[List[List[Int]]]{}

  implicitly[Ext[List[Int]]]                    // fails - not found
  implicitly[Ext[List[List[Int]]]]              // compiles
  implicitly[Ext[List[List[List[Int]]]]]        // fails - not found

  // Making Ext[+T] should incur the same behavior as these. (so says @paulp)
  implicitly[Ext[_ <: List[Int]]]               // compiles
  implicitly[Ext[_ <: List[List[Int]]]]         // compiles due to f and m having equally specific result types due
                                                // to scala/bug#10545 but f now being selected over m because it has
                                                // the longer implicit argument list. When the fix for 10545 is
                                                // merged this should compile, selecting m.
  implicitly[Ext[_ <: List[List[List[Int]]]]]   // compiles

  // But, we currently get:
  trait ExtCov[+T]
  implicitly[ExtCov[List[Int]]]                 // fails - not found
  implicitly[ExtCov[List[List[Int]]]]           // fails - not found
  implicitly[ExtCov[List[List[List[Int]]]]]     // fails - not found
}
