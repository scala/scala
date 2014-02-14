// exercise coevolveSym: SingleType with an underlying RefinedType
trait Thing { type A }
object IntThing extends Thing { type A = Int }

// The following erroneously failed with  error: method f overrides nothing.
// because asSeenFrom produced a typeref of the shape T'#A where A referred to a symbol defined in a T of times past
// More precisely, the TypeRef case of TypeMap's mapOver correctly modified prefix
// from having an underlying type of { type A = Ain } to { type A = Int }, with a new symbol for A (now with info Int),
// but the symbol in the outer type ref wasn't co-evolved (so it still referred to the { type A = AIn } underlying the old prefix)
// coEvolveSym used to only look at prefixes that were directly RefinedTypes, but they could also be SingleTypes with an underlying RefinedType
class View[AIn](val in: Thing { type A = AIn }) {          def f(p: in.A): in.A = p }
class SubView extends View[Int](IntThing)       { override def f(p: in.A): in.A = p }
