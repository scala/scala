
// scalac: --release:8 -Yrelease:java.lang
// javaVersion: 19+

// -Yrelease opens packages but does not override class definitions
// because ct.sym comes first

class C {
  def f(t: Thread) = t.threadId
}
