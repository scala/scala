class Node
object NodeSeq {
  implicit def seqToNodeSeq(s: Seq[Node]): NodeSeq = ???
}
abstract class NodeSeq extends collection.immutable.Seq[Node]

case class ant(t: String) extends scala.annotation.Annotation
object Test {
   def main(args: Array[String]): Unit = {
     val a: NodeSeq @ant("12") = Nil
     println(a)
   }
}
