import scala.collection.immutable.Map;
import scala.collection.immutable.TreeMap;
import scala.collection.immutable.ListMap;
import scala.collection.immutable.Order;

object Test with Application {
    val intOrder =
	Order.make((x:int,y:int) => x < y);

    test1();
    test2();
    Console.println("OK");



    def test1() = {
	val myMap:TreeMap[int,String] = new TreeMap(intOrder);
        test_map(myMap);
    }

    def test2() = {
	val myMap:ListMap[int,String] = new ListMap;
	test_map(myMap);
    }

    def test_map(myMap:Map[int,String]) = {
	val map1 = myMap.update(42,"The answer");
	val map2 = map1.update(17,"A small random number");
	val map3 = map2.update(666,"A bigger random number");
	val map4 = map3.update(4711,"A big random number");
	map1 == myMap + 42 -> "The answer";
	var i = 0;
	var map = map4;
	while(i < 43) {
	    map = map.update(i,i.toString());
	    i = i + 1;
	}
	i = 0;
	while(i < 4712) {
	    if(map.isDefinedAt(i))
		Console.print(i + "->" + map(i) + " ");
	    i = i + 1;
	}
	Console.println("");
    }
}


