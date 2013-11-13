// test well-kindedness checks -- arity error

class Monad[m[x]]

object mp extends Monad[Tuple2]
