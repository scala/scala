class A[U] { def f[T] = { class X extends A[T] } }


/*
$ scalac a.scala
$ javac -cp .:$SCALA_HOME/lib/scala-library.jar  -Xprint 'A$X$1'

  public class X$1 extends A<java.lang.Object> implements scala.ScalaObject {
    public X$1(A<U> null);
  }
*/