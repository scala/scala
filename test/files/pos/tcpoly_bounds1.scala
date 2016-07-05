case class T2[+T1, +T2](_1: T1, _2: T2) extends Product2[T1, T2]

class Foo[t[x]<: T2[Int, x]]

//
class MyPair[z](a: Int, b: z) extends T2[Int, z](a,b)

object foo extends Foo[MyPair]


trait Monad[m[x <: Bound[x]], Bound[x], a] // TODO: variances!
trait ListMonad[a] extends Monad[List, Any, a]

trait MyOrdered[a]
trait MySet[x <: MyOrdered[x]]
trait SetMonad[a <: MyOrdered[a]] extends Monad[MySet, MyOrdered, a]
