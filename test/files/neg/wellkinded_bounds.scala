// test well-kindedness checks -- syntax error
class WellKindedWrongSyntax[s <: List] { // must be s[x] <: List[x]
}
