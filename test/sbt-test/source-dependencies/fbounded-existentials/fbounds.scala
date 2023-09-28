class Dep {
  // The API representation for `bla`'s result type contains a cycle
  // (an existential's type variable's bound is the existential type itself)
  // This results in a stack overflow while showing the API diff.
  // Note that the actual result type in the compiler is not cyclic
  // (the f-bounded existential for Comparable is truncated)
  def bla(c: Boolean) = if (c) new Value else "bla"
}

class Value extends java.lang.Comparable[Value] { def compareTo(that: Value): Int = 1 }