// scalac: -Wconf:cat=migration:e -Xmigration -Xsource:3

// Demonstrate migration warnings at refchecks for -Xsource:3

class HasP {
  class P
}
class AnotherP extends HasP {
  class P extends super.P
}
class HasQ {
  class Q
}
class AnotherQ extends HasQ {
  class Q
}
