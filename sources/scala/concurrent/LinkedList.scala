package scala.concurrent;
class LinkedList[a] {
  var elem: a = _;
  var next: LinkedList[a] = null;
  def insert(elem: a): LinkedList[a] = {
    val nx = new LinkedList[a];
    nx.elem = elem;
    nx.next = next;
    next = nx;
    next
  }
}
