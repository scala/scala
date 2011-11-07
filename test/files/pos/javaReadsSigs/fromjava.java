import scala.*;
import scala.math.Ordering;
import scala.math.Numeric;
import scala.collection.Seq;
import scala.collection.Traversable;
import scala.collection.Traversable$;
import scala.collection.immutable.Set;
import scala.collection.immutable.HashSet;
import scala.collection.immutable.Map;
import scala.collection.immutable.Map$;
import scala.collection.immutable.HashMap;
import scala.collection.immutable.Vector;
import scala.collection.immutable.List;
import scala.collection.generic.CanBuildFrom;

class A { };
class B { };

// This one compiles but it would be better if it didn't.
// Checking in under pos anyway in the interests of making sure
// we are informed if the status changes.
class Contra {
  // Not an Ordering<Character>.
  static Ordering<Object> charOrd = scala.math.Ordering$Char$.MODULE$;
  
  public boolean useCharOrd() {
    return charOrd.compare(new Object(), new Object()) == 0;
  }
  
  static Numeric<?> intNum = scala.math.Numeric$IntIsIntegral$.MODULE$;
}

public class fromjava {
  public static Function1<A, B> f1 = new scala.runtime.AbstractFunction1<A, B>() {
    public B apply(A a) {
      return null;
    }
  };
  
  public static Function1<Tuple2<? extends Object, B>, B> f2 = new scala.runtime.AbstractFunction1<Tuple2<? extends Object, B>, B>() {
    public B apply(Tuple2<? extends Object, B> tup) {
      return tup._2();
    }
  };
  
  public static String vector(Vector<String> x) {
    Vector<String> y = x.take(2);
    return y.head();
  }
  public static String list(List<String> x) {
    List<String> y = x.drop(2);
    return y.head();
  }
  public static Tuple2<String, Integer> map(Map<String, Integer> x) {
    Traversable<Tuple2<String, Integer>> y = x.drop(2);
    return y.head();
  }
  public static <T> Object sum(Traversable<T> x) {
    return x.sum(Contra.intNum);
  }
  // Looks like sum as given below fails under java5, so disabled.
  //
  // [partest] testing: [...]/files/pos/javaReadsSigs                                [FAILED]
  // [partest] files/pos/javaReadsSigs/fromjava.java:62: name clash: sum(scala.collection.Traversable<A>) and <T>sum(scala.collection.Traversable<T>) have the same erasure
  // [partest]   public static B sum(Traversable<A> x) {
  // [partest]                   ^
  //
  //
  // can't make this work with an actual CanBuildFrom: see #4389.
  // public static B sum(Traversable<A> x) {
  //   // have to cast it unfortunately: map in TraversableLike returns
  //   // "That" and such types seem to be signature poison.
  //   return ((Traversable<B>)x.map(f1, null)).head();
  // }
}