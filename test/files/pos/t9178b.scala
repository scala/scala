abstract class Test{
  val writeInput: java.io.OutputStream => Unit
  def getOutputStream(): java.io.OutputStream

  writeInput(getOutputStream)
}

