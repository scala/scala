class Top[A] {
	type AType = A
}

trait Node extends NotNull { outer =>
	type T <: Node
	def prepend = new Node { type T = outer.type }
}

class Main[NextType <: Node](value: Node { type T = NextType })
	extends Top[Node { type T = NextType }] {
	
	new Main[AType]( (value: AType).prepend )
}
