class GenericsCompilerCrashTest {
  def test() {
    Methods.acceptGenericInterface(Methods.getGenericInterface())
  }
}