

I have a trait:

trait Vis[+T]

and an object/class pair:

object VisImpl? { def apply() = new VisImpl? } class VisImpl? extends Vis[Missing]

Where Missing is some class of mine. In a separate project (where Vis and VisImpl? are on the classpath but Missing is not), if I do:

object Test extends Application {

    val v = VisImpl?() println(v)

}

This causes a Scala compiler error (using 2.7.5 compiler). The error is:

"Caused by java.lang.RuntimeException?: malformed Scala signature of VisImpl? at 3634; reference value data of package mypack refers to nonexisting symbol"

Where mypack is the root package of the Missing class. This is not a helpful error as all my classes share the same root package and the problem is not in the VisImpl? declaration in any case.

I would expect to see an error of the form:

" Type parameter not found 'Missing': VisImpl? extends Vis[Missing] at Test: #4: val v = VisImpl?() "
