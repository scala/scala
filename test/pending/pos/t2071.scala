/**
 * We still have to evaluate whether we will permit existentials
 * with cross type dependencies. My current reaction would be no.
 * Ticket stays open until a decision is made.
 */
trait Iterable[+S]
trait Box[U]

trait A {
 type T <: Iterable[S] forSome { type S <: Box[U]; type U }
}

trait B extends A {
 type T <: Iterable[S] forSome { type S <: Box[U]; type U }
}
/*
But according to SLS, 3.5.1 Type Equivalence: Two existential types (§3.2.10) are equivalent if they have the same number of quantifiers, and, after renaming one list of type quantifiers by another, the quantified types as well as lower and upper bounds of corresponding quantifiers are equivalent.

So, every existential type must be equivalent to (and conform to) itself.
Attachments
*/
