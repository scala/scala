trait Parent {
 type Test[A, H[B <: A]]
}
trait Sub extends Parent {
 type Test[AS, HS[B <: AS]] = AS
}