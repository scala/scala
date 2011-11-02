object Test {
import scala.xml._;

  def bar(args: Seq[String]) = args match {
    case Seq(a,b,c,d @ _*) => Console.println("cool!")
    case _ => Console.println("bah")
  }
  def foo(args: List[String]) = 
    Elem(null,"bla",Null, TopScope, (args map {x => Text(x)}):_*) match {
      case Elem(_,_,_,_,Text("1"),_*) =>
        Console.println("cool!")
      case _ =>
        Console.println("bah")
    }
  
  def main(args: Array[String]) = {
    val li = List("1","2","3","4")
    bar(li)
    foo(li)
  }
}
