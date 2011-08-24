// was t1001.scala
class Foo;

object Overload{
  val foo = classOf[Foo].getConstructors()(0)
  foo.getDeclaringClass
}

// was t1001.scala

// I suspect the stack overflow is occurring when the compiler is determining the types for the following line at the end of the file:-
//    val data = List(N26,N25)

abstract class A
{
    // commenting out the following line (only) leads to successful compilation
    protected val data: List[A]
}

trait B[T <: B[T]] extends A { self: T => }

abstract class C extends A
{
    // commenting out the following line (only) leads to successful compilation
    protected val data: List[C]
}

abstract class D extends C with B[D] {}

abstract class Ee extends C with B[Ee]
{
}


object N1 extends D
{
    val data = Nil
}

object N2 extends D
{
    val data = Nil
}

object N5 extends D
{
    val data = List(N1)
}

object N6 extends D
{
    val data = List(N1)
}

object N8 extends D
{
    val data = List(N1)
}

object N10 extends D
{
    val data = Nil
}

object N13 extends D
{
    val data = List(N2)
}

object N14 extends D
{
    val data = List(N5,N10,N8)
}

object N15 extends D
{
    val data = List(N14)
}

object N16 extends D
{
    val data = List(N13,N6,N15)
}

object N17 extends D
{
    val data = List(N16)
}

object N21 extends D
{
    val data = List(N16)
}

object N22 extends D
{
    val data = List(N17)
}

object N25 extends D
{
    val data = List(N22)
}

object N26 extends Ee
{
    val data = List(N21,N17)
}

// Commenting out the following object (only) leads to successful compilation
object N31 extends Ee
{
    // If we use List[C](N26,N25), we achieve successful compilation
    val data = List[C](N26,N25)
}
