// this is a run-test because the compiler should emit bytecode that'll pass the JVM's verifier
object Test extends Application {
  def `(` = error("bla")
  def `.` = error("bla")
  def `)` = error("bla")
  def `,` = error("bla")
}
