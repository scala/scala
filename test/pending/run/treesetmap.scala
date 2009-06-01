object Test{
  def main(args : Array[String]){
    println(scala.collection.immutable.TreeSet(1, 2, 3).map(x => x).getClass);
  }
}
