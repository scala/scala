module test {

  def error[a](x: String):a = new java.lang.RuntimeException(x) throw;

  def main = error("hi!");
}