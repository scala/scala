// tested using Scala compiler version 2.4.0-RC1 -- (c) 2002-2011 LAMP/EPFL

// Many thanks to all at LAMP for the work that goes into Scala.


class A {}

trait B[T <: B[T]] { self: T => }

abstract class C extends A with B[C]
{
    protected val data: List[Int]
}

class E extends D
{
    val data = Nil
}

class F extends D
{
    val data = Nil
}

abstract class D extends C with B[D] {}
