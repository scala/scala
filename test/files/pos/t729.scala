import scala.language.implicitConversions

// implicit must be accessible without a prefix,
// which is true for the overloaded `coerce` method.
// The superclass method is not hidden,
// as shown by the explicit call.
// This has been a neg test since 2.3 because
// of erroneous shadowing logic when inferring implicits.
//
// The issue is not in the current tracker.
// https://github.com/scala/scala/commit/37b3648e30cbce8ac9f2b9b48b63cdf15d6a2d34

trait Parser {
  type Node <: NodeImpl
  implicit def coerce(n: NodeImpl): Node = n.self
  trait NodeImpl {
    def self: Node
  }
  trait Link {
    def from: NodeImpl
  }
}

trait ScalaParserAutoEdit extends Parser {
  type Node <: NodeImpl
  implicit def coerce(node: NodeImpl): Node = node.self
  type SN = super[Parser].NodeImpl
  trait NodeImpl extends super[Parser].NodeImpl {
    def self: Node
    def foo = {
      var link: Link = null
      val xxx: NodeImpl = coerce(link.from)
      //implicit val f: SN => Node = coerce
      val yyy: NodeImpl = link.from
    }
  }
}
