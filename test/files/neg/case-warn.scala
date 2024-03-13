//> using options -Werror -Xsource:3 -Wconf:cat=scala3-migration:w

// warn about case class synthetic method getting access modifier from constructor.
// if erroring, the error message for apply is hidden by previous error for copy.

case class C private (c: Int)
