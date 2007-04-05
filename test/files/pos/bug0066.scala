class GBTree[A, B] /*with Map[A, B, GBTree[A,B]]*/ {
    abstract class Tree[A,B];
    case class Node[A,B](key:A,value:B,smaller:Node[A,B],bigger:Node[A,B])
	extends Tree[A,B];
    case class Nil[A,B]() extends Tree[A,B];

}
