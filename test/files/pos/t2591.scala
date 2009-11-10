class A
class B

object Implicits {
  implicit def imp(x: A): Int = 41
  implicit def imp(x: B): Int = 41
}

object Test {
	// should cause imp to be in scope so that the next expression type checks
	// `import Implicits._` works
	import Implicits.imp

  (new A) : Int
}