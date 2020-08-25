// scalac: -Xsource:2.10 -deprecation -language:higherKinds -Xfatal-warnings
class Foo[+CC[X]] { type Coll = CC[_] }
