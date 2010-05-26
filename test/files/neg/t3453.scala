// test shadowing of implicits by synonymous non-implicit symbols
// whether they be inherited, imported (explicitly or using a wildcard) or defined directly
class A
class B

trait S {
    implicit def aToB(a: A): B = new B
}

class T1 extends S {
    def x: B = {
        val aToB = 3
        // ok: doesn't compile, because aToB method requires 'T.this.' prefix
        //aToB(new A)

        // bug: compiles, using T.this.aToB,
        //   despite it not being accessible without a prefix
        new A
    }
}

object O {
    implicit def aToB(a: A): B = new B
}

class T2a {
    import O._

    def x: B = {
        val aToB = 3
        // ok: doesn't compile, because aToB method requires 'T.this.' prefix
        //aToB(new A)

        // bug: compiles, using T.this.aToB,
        //   despite it not being accessible without a prefix
        new A
    }
}

class T2b {
    import O.aToB

    def x: B = {
        val aToB = 3
        // ok: doesn't compile, because aToB method requires 'T.this.' prefix
        //aToB(new A)

        // bug: compiles, using T.this.aToB,
        //   despite it not being accessible without a prefix
        new A
    }
}

class T3 {
    implicit def aToB(a: A): B = new B

    def x: B = {
        val aToB = 3
        // ok: doesn't compile, because aToB method requires 'T.this.' prefix
        //aToB(new A)

        // bug: compiles, using T.this.aToB,
        //   despite it not being accessible without a prefix
        new A
    }
}