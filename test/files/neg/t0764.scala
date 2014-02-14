class Top[A] {
        type AType = A
}

trait Node { outer =>
        type T <: Node
        def prepend = new Node { type T = outer.type }
}

class Main[NextType <: Node](value: Node { type T = NextType })
        extends Top[Node { type T = NextType }] {

        new Main[AType]( (value: AType).prepend )
}

/* we've been back-and-forth on this one -- see PRs on SI-8177 for the reasoning
I think it should compile and that the following error is due to broken =:= on existentials
 found   : Node{type T = _1.type} where val _1: Node{type T = NextType}
 required: Node{type T = Main.this.AType}
    (which expands to)  Node{type T = Node{type T = NextType}}

I claim (omitting the forSome for brevity, even though the premature skolemization is probably the issue)
_1.type =:= Main.this.AType
because
(1) _1.type <:< Main.this.AType and (2) Main.this.AType  <:< _1.type
(1), because:
_1.type <:< Node{type T = NextType} (because skolemization and _1's upper bound)
(2), because:
Node{type T = NextType} <:< _1.type forSome val _1: Node{type T = NextType}
because:
Node{type T = NextType} <:< T forSome {type T <: Node{type T = NextType} with Singleton}
because 
Node{type T = NextType} <:< Node{type T = NextType} with Singleton

hmmm.. might the with Singleton be throwing a wrench in our existential house?

Behold the equivalent program which type checks without the fix for SI-8177.
(Expand type alias, convert type member to type param;
note the covariance to encode subtyping on type members.)

class Node[+T <: Node[_]] { def prepend = new Node[this.type] }
class Main[NextType <: Node[_]](value: Node[NextType]) {
  new Main(value.prepend)
}
*/