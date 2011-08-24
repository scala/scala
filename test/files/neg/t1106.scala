class Par[S]
val p = new Par[String]
class Foo[T[x]<:Par[x]](t: T[String])

new Foo[p.type](p) // crashes compiler
