final class Option[+A](val value: A) extends AnyVal

// Was: sandbox/test.scala:21: error: bridge generated for member method f: ()Option[A] in class Bar
//      which overrides method f: ()Option[A] in class Foo" 
abstract class Foo[A]                { def f(): Option[A] }
         class Bar[A] extends Foo[A] { def f(): Option[A] = ??? }

// User reported this as erroneous but I couldn't reproduce with 2.10.{0,1,2,3}
// https://github.com/scala/bug/issues/6260#issuecomment-292410477
// I suspect he whittled down the example too far.
class Wrapper(val value: Int) extends AnyVal
abstract class Test { def check(the: Wrapper): Boolean }
object T {
  new Test { def check(the: Wrapper) = true }
}
