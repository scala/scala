//> using options -Xfatal-warnings
//
object Test {
  try { ??? } // warn

  try { ??? } finally ??? // no warn
  try { ??? } catch { case _: Throwable => } // no warn
}
