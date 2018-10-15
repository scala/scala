// scalac: -Xfatal-warnings -unchecked
//
class Test {
  Option.whenNonNull(3) match { case Some(n) => n; case None => 0 }
}
