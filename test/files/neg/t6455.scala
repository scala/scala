object O { def filter(p: Int => Boolean): O.type = this }

class Test {
  // should not compile because we no longer rewrite withFilter => filter
  O.withFilter(f => true)
}
