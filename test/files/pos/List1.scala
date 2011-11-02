object lists {

  abstract class List[a] {
      def isEmpty: Boolean;
      def head: a;
      def tail: List[a];
      def prepend(x: a) = Cons[a](x, this);
  }

  def Nil[b] = new List[b] {
    def isEmpty: Boolean = true;
    def head = error("head of Nil");
    def tail = error("tail of Nil");
  }

  def Cons[c](x: c, xs: List[c]): List[c] = new List[c] {
    def isEmpty = false;
    def head = x;
    def tail = xs;
  } 

  def foo = {
    val intnil = Nil[Int];
    val intlist = intnil.prepend(1).prepend(1+1);
    val x: Int = intlist.head;
    val strnil = Nil[String];
    val strlist = strnil.prepend("A").prepend("AA");
    val y: String = strlist.head;
    ()
  }

  class IntList() extends List[Int] {
    def isEmpty: Boolean = false;
    def head: Int = 1;
    def foo: List[Int] { def isEmpty: Boolean; def head: Int; def tail: List[Int] } = Nil[Int];
    def tail0: List[Int] = foo.prepend(1).prepend(1+1);
    def tail: List[Int] = Nil[Int].prepend(1).prepend(1+1);
  }

  def foo2 = {
    val il1 = new IntList();
    val il2 = il1.prepend(1).prepend(2);
    ()
  }
}
