object Test {
 def g[S, T <: S](s: S)(t: T): Unit = println("")
 g("a")("a") // error: inferred type arguments [java.lang.String] do not conform to method g's type parameter bounds [T <: S]
}