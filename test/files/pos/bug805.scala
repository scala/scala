trait MatcherYYY  {
  trait NodeImpl;
  trait Matchable extends NodeImpl {
    protected def doMatch : Unit = {}
  }
}
trait BraceMatcherXXX extends MatcherYYY {
  trait NodeImpl extends super.NodeImpl {
    def doMatch  (braces : BracePair) : Unit
  }
  trait BracePair {
    trait BraceImpl extends NodeImpl with Matchable {
      override def doMatch : Unit = {
        super.doMatch;
        ();
      }
    }
  }
}
