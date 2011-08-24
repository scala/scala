package test;
trait Test {
  type Matchable <: Node with MatchableImpl;
  trait MatchableImpl extends NodeImpl {
    def asMatch(node : Matchable) : Any;
  }
  type Node <: NodeImpl;
  trait NodeImpl;
  trait CoreIfImpl extends MatchableImpl {
    // NO_CRASH: def asMatch(m : Matchable) = {
    def asMatch(m : Node) : Any = {
      if (m.isInstanceOf[MatchableImpl]) {
        null;
      } else null;
      // NO_CRASH: null;
    }
  }
}
