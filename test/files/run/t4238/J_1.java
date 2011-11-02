import scala.*;

class J_1 {
	scala.collection.mutable.HashMap<String, String> map =
		new scala.collection.mutable.HashMap<String, String>();
	
	Function1<Tuple2<String, String>, Integer> f =
	  new scala.runtime.AbstractFunction1<Tuple2<String, String>, Integer>() {
	    public Integer apply(Tuple2<String, String> s) {
	      return s._1().length();
	    }
    };
		
	scala.collection.Seq<Integer> counts =
	  map.groupBy(f).keys().toList();
}
