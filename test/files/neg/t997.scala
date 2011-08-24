// An extractor with 2 results
object Foo { def unapply(x : String)  = Some(Pair(x, x)) }

object Test extends App {

// Prints 'x'; ought not to compile (or maybe a should be the Pair?).
"x" match { case Foo(a) => Console.println(a) }

// Prints '(x,x)' as expected.
"x" match { case Foo(a, b) => Console.println((a,b)) }

// Gives confusing error 'not found: value c'.
"x" match { case Foo(a, b, c) => Console.println((a,b,c)) }

}
