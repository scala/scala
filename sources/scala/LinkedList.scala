package scala;

class LinkedList[a]() {
  var elem: a = _;
  var next: LinkedList[a] = null;
  def append(x: a): LinkedList[a] = {
    val l = new LinkedList[a]();
    l.elem = x;
    this.next = l;
    l
  }
}

