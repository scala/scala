class D[A <: Ordered[A],B] {
    abstract class Tree[A <: Ordered[A],B]();
    case class Node[A,B](key:A,value:B,smaller:Tree[A,B],bigger:Tree[A,B])
	extends Tree[A,B];
    case class Nil[A,B]() extends Tree[A,B];


    abstract case class InsertTree[A,B]();
    case class ITree[A,B](t:Tree[A,B]); // forgot extends InsertTree[A,B];
    case class INode[A,B](t:Tree[A,B],height:int,size:int); // forgot extends InsertTree[A,B];
    val tree:Tree[A,B] = Nil();
    val s = 0;

    def insert(key:A, value:B) = {
	val ITree(t1) = insert_1(key, value, tree, s);
    }

    def insert_1(key:A, value:B, t0:Tree[A,B], s:int):InsertTree[A,B] = {
	INode(t0,1,s);
    }
}
