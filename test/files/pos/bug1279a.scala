// see #13
// providing the type parameter in the recursive call to all4Impl
// avoids the problem


// covariant linked list
abstract class M
{ self =>

    type T
    final type selfType = M {type T <: self.T}
    type actualSelfType >: self.type <: selfType

    def next: selfType


    // I don't understand why this doesn't compile, but that's a separate matter
    // error: method all2 cannot be accessed in M.this.selfType
    // because its instance type => Stream[M{type T <: M.this.selfType#T}]
    // contains a malformed type: M.this.selfType#T
    // def all2: Stream[M {type T <: self.T}] = Stream.cons(self: actualSelfType, next.all2)


    // compiles successfully
    // def all3: Stream[M {type T <: self.T}] = all3Impl(self: actualSelfType)
    // private def all3Impl(first: M {type T <: self.T}): Stream[M {type T <: self.T}] = Stream.cons(first, all3Impl(first.next))



    def all4: Stream[M {type T <: self.T}] = Unrelated.all4Impl[T](self: actualSelfType)
}

object Unrelated
{
  // TODO!!! fix this bug for real, it compiles successfully, but weird types are inferred
   // def all4Impl[U](first: M {type T <: U}): Stream[M {type T <: U}] = Stream.cons(first, all4Impl(first.next))

// compiles successfully
     def all4Impl[U](first: M {type T <: U}): Stream[M {type T <: U}] = Stream.cons(first, all4Impl[U](first.next))
}
