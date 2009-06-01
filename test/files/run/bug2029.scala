object Test{
  def main(args : Array[String]){
    import scala.collection.immutable.TreeSet;

    val mainSet = TreeSet(1 to 5 :_*)

    var compareCalled = false;
    val smallerSet = TreeSet(2 to 4 :_*)(Ordering[Int].reverse)

    println(mainSet.mkString(","))
    println(smallerSet.mkString(","))
    println(smallerSet.subsetOf(mainSet));
  }


}
