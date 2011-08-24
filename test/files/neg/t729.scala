trait Parser {
  type Node <: NodeImpl;
  implicit def coerce(n : NodeImpl) = n.self;
  trait NodeImpl {
    def self : Node;
  }
  trait Link {
    def from : NodeImpl;
  }
}

trait ScalaParserAutoEdit extends Parser {
  type Node <: NodeImpl;
  implicit def coerce(node : NodeImpl) = node.self;
  trait NodeImpl extends super[Parser].NodeImpl {
    def self : Node;
    def foo = {
      var link : Link = null;
      val xxx : NodeImpl = coerce(link.from);
      val yyy : NodeImpl = link.from;
    }
  }
}
