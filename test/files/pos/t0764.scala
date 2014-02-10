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

/* this used to be a neg test, even though it should've compiled
SI-8177 fixed this.

Behold the equivalent program which type checks without the fix for SI-8177.
(Expand type alias, convert type member to type param;
note the covariance to encode subtyping on type members.)

class Node[+T <: Node[_]] { def prepend = new Node[this.type] }
class Main[NextType <: Node[_]](value: Node[NextType]) {
  new Main(value.prepend)
}
*/