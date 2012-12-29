// this is a run-test because the compiler should emit bytecode that'll pass the JVM's verifier
// See https://blogs.oracle.com/jrose/entry/symbolic_freedom_in_the_vm for more information
// on dangerous characters.
object Test extends App {
  def `.` = ???
  def `,` = ???
  def `:` = ???
  def `;` = ???
  def `/` = ???
  def `\\`= ???
  def `(` = ???
  def `)` = ???
  def `[` = ???
  def `]` = ???
  def `<` = ???
  def `>` = ???
  def `\u0000` = ???
}
