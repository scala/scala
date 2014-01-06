
// alias
trait Thingy

class Gramps

// sibling classes that extend a case class
case class Thing(i: Int) extends Gramps
class ThingOne(x:Int) extends Thing(x)
class ThingTwo(y:Int) extends Thing(y) with Thingy
final class ThingThree(z:Int) extends Thing(z)

// not case cousin
class Cousin extends Gramps

class SimpleParent
case class Simple() extends SimpleParent
case object SimpleSibling extends SimpleParent

// value classes
final class ValueClass1(val value: Int) extends AnyVal
final class ValueClass2[T](val value: T) extends AnyVal
final case class ValueClass3(val value: Int) extends AnyVal

/* It's not possible to run partest without -deprecation.
 * Since detecting the warnings requires a neg test with
 * -Xfatal-warnings, and deprecation terminates the compile,
 * we'll just comment out the nasty part.  The point was
 * just to show there's nothing special about a trait
 * that extends a case class, which is only permitted
 * (deprecatingly) by omitting the parens.
 *
// common ancestor is something else
class AnyThing
case class SomeThing extends AnyThing // deprecation
class OtherThing extends AnyThing

// how you inherit caseness doesn't matter
trait InThing extends SomeThing
class MyThing extends InThing
*/

object Test {
  def main(a: Array[String]) {
    // nothing to do with Gavin
    println(new Some(1) == new Some(1)) // OK, true
    println(new Some(1) == None) // Should complain on type, was: spuriously complains on fresh object
    println(Some(1) == new Thing(1)) // Should complain on type, was: spuriously complains on fresh object

    val t1 = new ThingOne(11)
    val t2: Thingy = new ThingTwo(11)
    val t3 = new ThingTwo(11)
    val t4 = new ThingThree(11)
    val c = new Cousin

    println(t1 == t2) // true, but apparently unrelated, a compromise warning
    println(t4 == t2) // true, complains because ThingThree is final and Thingy not a subclass, stronger claim than unrelated
    println(t2 == t3) // OK, two Thingy
    println(t3 == t2) // ditto with case receiver
    println(t3 == Some(1)) // false, warn on different cases
    println(t1 == c) // should warn

    // don't warn on fresh cases
    println(new ThingOne(11) == t1) // OK, was: two cases not warnable on trunk
    println(new ThingTwo(11) == t2) // true, was: spuriously complains on fresh object
    println(new ThingOne(11) == t3) // two cases not warnable on trunk
    println(new ThingTwo(11) == t3) // ditto

    println(new Simple() == SimpleSibling) // like Some(1) == None, but needn't be final case

    println(new ValueClass1(5) == new ValueClass1(5)) // ok
    println(new ValueClass1(5) == 5) // bad
    println(new ValueClass1(5) == (5: Any)) // ok, have to let it through
    println(5 == new ValueClass1(5)) // bad
    println((5: Any) == new ValueClass1(5) == (5: Any)) // ok

    println(new ValueClass2("abc") == new ValueClass2("abc")) // ok
    println(new ValueClass2("abc") == "abc") // bad
    println(new ValueClass2(5) == new ValueClass1(5)) // bad - different value classes
    println(ValueClass3(5) == new ValueClass3(5)) // ok
    println(ValueClass3(5) == new ValueClass2(5)) // bad
    println(ValueClass3(5) == 5) // bad

    /*
    val mine = new MyThing
    val some = new SomeThing
    val other = new OtherThing
    println(mine == some) // OK, two Something
    println(some == mine)
    println(mine == other) // OK, two Anything?
    println(mine == t1) // false
    */
  }
}
