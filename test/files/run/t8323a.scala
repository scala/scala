trait Node { // must be a trait
  def isLeaf: Boolean = ???
}

object MyLeaf extends Node {
  override final val isLeaf = true // must be override final val
  // mixin erroneously generates two methods in the object: forwarder and the actual method
  /** (un-mangled from a repl session)
  object MyLeaf extends Object with Node {
    def isLeaf(): Boolean = Node$class.isLeaf(MyLeaf.this);
    final override <stable> <accessor> def isLeaf(): Boolean(true) = true;
  */
}

object Test {
  def main(args: Array[String]) {
    MyLeaf
  }
}