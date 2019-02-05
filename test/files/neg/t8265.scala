// scalac: -deprecation -language:higherKinds
//
class Foo[+CC[X]] { type Coll = CC[_] }
