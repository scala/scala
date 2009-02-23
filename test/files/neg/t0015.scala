abstract class Test
{
   def f: Nothing

   Nil.map(f _)
}

abstract class M
{ self =>

    type T
    final type selfType = M {type T = self.T}
    type actualSelfType >: self.type <: selfType


    def f[U](x: Any) = {}

    // compiles successfully
    //f[Int](self: actualSelfType)

    f[Int](self: selfType) // compiles Ok now was well, because we narrow to singletonType in this situation

    //def g(x: Any) = {}
    //g(self: selfType)
}
