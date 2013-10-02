trait Matcher {
  trait Link {
    type Self <: Link;
    type Match <: Link { type Match = Link.this.Self; }
  }
  trait HasLinks {
    def link(b : Boolean) : Link = null;
  }

}
trait BraceMatcher extends Matcher {
  trait BracePair {
    trait BraceLink extends Link;
    trait OpenLink extends BraceLink {
      type Self = OpenLink;
      type Match = CloseLink;
    }
    trait CloseLink extends BraceLink {
      type Self = CloseLink;
      type Match = OpenLink;
    }
  }
}
trait IfElseMatcher extends BraceMatcher {
  trait IfElseLink extends Link;
  trait IfLink extends IfElseLink {
    type Self = IfLink;
    type Match = ElseImpl;
  }
  trait ElseImpl extends IfElseLink with HasLinks {
    type Self = ElseImpl;
    type Match = IfLink;
    override def link(b : Boolean) = this;
  }
  val parenPair : BracePair;
  trait IfWithParenImpl extends HasLinks {
    object ifLink extends IfLink;
    object openParen extends parenPair.OpenLink;
    override def link(b : Boolean): Link = b match {
    case true => ifLink;
    case false => openParen;
    }
  }
}

