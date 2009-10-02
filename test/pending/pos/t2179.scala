on trunk r18368, this delightful little poison pill:

(Nil:List[List[Double]]).reduceLeft((_: Any, _: Any) => Nil.indices.map(_ => 0d))

sends the compiler into an apparently infinite loop. a sample thread dump shows:

