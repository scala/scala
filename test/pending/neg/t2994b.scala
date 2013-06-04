trait curry[s[_]] { type f = Double }

// a1 and a2 fail to compile, but all three should fail.
class A {
  type a1[s[_ <: Int]] = curry[s]
  type a2[s[_ <: Int]] = curry[s]#f
  type a3[s[_ <: Int]] = Set[curry[s]#f]
}
