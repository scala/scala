package scala.runtime.compat;

object Platform {
  def getClass(obj: AnyRef) = obj.getClass();
  def getClassName(obj: AnyRef) = obj.getClass().getName();
  def printStackTrace(exc: java.lang.Exception) = exc.printStackTrace();
  def getMessage(exc: java.lang.Exception) = exc.getMessage();
  def split(str: String, separator: Char): Array[String] = {
    str.split(separator.toString());
  }
}
