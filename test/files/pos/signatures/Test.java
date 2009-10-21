import scala.collection.mutable.IndexedSeq;
import test.Outer;

/* Test correct generation of java signatures. The Outer class should not
 * have a Java signature attribute for the inner method definition. Trait
 * Mutable should have one, even though it is also a nested definition.
 * (but for classes there is a way to tell about nesting to the JVM).
 */
class Test {
    Outer o = new Outer();
}
