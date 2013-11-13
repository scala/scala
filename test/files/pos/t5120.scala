// An example extracted from SBT by Iulian
// that showed that the previous fix to t5120
// was too strict.
class Test {
  class ScopedKey[T]
  class Value[T]

  class Compiled[T](val settings: Seq[Tuple2[T]])

  case class Tuple2[T](k: ScopedKey[T], v: ScopedKey[T])

  def transform[T](x: T) = x

  def test(compiledSettings: Seq[Compiled[_]]) =  {
    compiledSettings flatMap { cs => // cd: Compiled[_] in both versions
      (cs.settings  map { s =>       // cs.settings: Seq[Compiled[$1]] in trunk, Seq[Compiled[$1]] forSome $1 in 2.9.1
                                     // s: Pair[$1] in trunk, Pair[$1] in 2.9.1
        val t = transform(s.v)       // t: ScopedKey[_] in trunk, ScopedKey[$1] in 2.9.1
        foo(s.k, t)
        t
     }) : Seq[ScopedKey[_]]
    }
  }

  def foo[T](x: ScopedKey[T], v: ScopedKey[T]) {}
}
