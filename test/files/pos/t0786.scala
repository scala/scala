object ImplicitProblem {
    class M[T]

    def nullval[T] = null.asInstanceOf[T];

    trait Rep[T] {
        def eval: int
    }

    implicit def toRep0(n: int) = new Rep[int] {
        def eval = 0
    }

    implicit def toRepN[T](n: M[T])(implicit f: T => Rep[T]) = new Rep[M[T]] {
        def eval = f(nullval[T]).eval + 1
    }

    def depth[T <% Rep[T]](n: T) = n.eval

    def main(args: Array[String]) {
        println(depth(nullval[M[int]]))  // (1) this works
        println(nullval[M[int]].eval)    // (2) this works

        type m = M[int]
        println(depth(nullval[m]))     // (3) this doesn't compile on 2.7.RC1
        println(nullval[m].eval)       // (4) this works
    }

}
