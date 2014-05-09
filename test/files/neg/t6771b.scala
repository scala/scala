// Currently, the pattern matcher widens the type of the
// scrutinee, so this doesn't typecheck. This test just
// confirms this behaviour, although it would be an improvement
// to change this and make this a `pos` test.
//
// But, to the intrepid hacker who works on this, a few notes:
// You'll have to look into places in the pattern matcher that
// call `dealias`, and see if they need to be `dealiasWiden`.
object Test {
  val a = ""; var b: a.type = a

  b = b match { case x => x }
}

