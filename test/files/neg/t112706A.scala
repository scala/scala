package test;
trait Test {
  def foo(p : List[Tuple2[String,String]]) = {
    for (t <- p) t._1 match {
    case Tuple2(node,_) =>   
    }
  }
}
