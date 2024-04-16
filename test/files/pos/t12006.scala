//> using options -Xsource:3

// see https://github.com/scala/bug/issues/12006
// java.io.InputStream looks like a SAM (read method),
// but u.openStream returns InputStream so don't eta-expand.
class C1(s: => java.io.InputStream)
class D1(u: java.net.URL) extends C1(u.openStream) // ok

class C2(s: java.io.InputStream)
class D2(u: java.net.URL) extends C2(u.openStream) // ok
