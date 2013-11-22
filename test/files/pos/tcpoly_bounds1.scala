class Foo[t[x]<: Tuple2[Int, x]]

//
class MyPair[z](a: Int, b: z) extends Tuple2[Int, z](a,b)

object foo extends Foo[MyPair]


trait Monad[m[x <: Bound[x]], Bound[x], a] // TODO: variances!
trait ListMonad[a] extends Monad[List, Any, a]

trait MyOrdered[a]
trait MySet[x <: MyOrdered[x]]
trait SetMonad[a <: MyOrdered[a]] extends Monad[MySet, MyOrdered, a]
