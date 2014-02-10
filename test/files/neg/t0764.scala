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

/* TODO: this should compile with a proper fix for SI-8177

Behold the equivalent program which already type checks.
(Expand type alias, convert type member to type param;
note the covariance to encode subtyping on type members.)

class Node[+T <: Node[_]] { def prepend = new Node[this.type] }
class Main[NextType <: Node[_]](value: Node[NextType]) {
  new Main(value.prepend)
}
*/