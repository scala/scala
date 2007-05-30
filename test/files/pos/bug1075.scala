class Directory(var dir_ : String)
{
  if (!dir_.startsWith("/")) {
    throw new RuntimeException("Invalid directory")
  }
  dir_ = dir_.replaceAll("/{2,}", "/")

  def this(serialized : Array[byte]) = {
    this(new String(serialized, "UTF-8"))
  }

  def dir = dir_
}

object Test extends Directory("/bab/dkkd//dkkdkd//kdkdk") with Application {
  println(dir)
}
