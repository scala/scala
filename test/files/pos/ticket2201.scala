class Test
object Test { implicit def view(x : Test) = 0 }

object Call {
  def call(implicit view : Test => Int) = view(null)
  call
  call
}
