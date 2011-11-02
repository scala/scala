package trials
object crashing {
  trait C {
    abstract class T[A] {
      def f[Z] (a:T[Z]) : T[A]
    }
  }
  abstract class Thing {
    val c = new C{
      class T[A](a:A) {
        def f[Z](t:T[Z]) = new T(a)
      }
    }
    val x1 = {
      class C[T] { val x: T }
      new C[String]
    }
  }
}
/* 

Infinite loop in Typer.addLocals. Printing all calls to it:

addLocals: Unit
addLocals: this.T[A]
addLocals: java.lang.Object with crashing.C{ ... }
addLocals: >: Nothing <: java.lang.Object with crashing.C{type T[A] <: java.lang.Object with ScalaObject{def f[Z](this.T[Z]): this.T[A]}}
addLocals: >: Nothing <: java.lang.Object with ScalaObject{def f[Z](this.T[Z]): this.T[Z]}
addLocals: >: Nothing <: java.lang.Object with ScalaObject{def f[Z](this.T[Z]): this.T[Z]}
addLocals: >: Nothing <: java.lang.Object with ScalaObject{def f[Z](this.T[Z]): this.T[Z]}
[...]
*
  C { type T[A] <: { def f[Z]: T[Z] => T[A] } }

*/
