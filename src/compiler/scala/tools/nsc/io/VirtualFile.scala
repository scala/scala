package scala.tools.nsc.io;

class VirtualFile(val name : String, val path : String) extends AbstractFile {
  assert(name != null && path != null, name + " - " + path);
  def this(name : String) = this(name, name);
  def file = null;
  def isDirectory = false;
  def lastModified = java.lang.Long.MIN_VALUE;

  def read = {
    assert(!isDirectory);
    new Array[Byte](0);
  }

  def list : List[AbstractFile] = {
    assert(isDirectory);
    Nil;
  }

  def lookupName(name : String, isDirectory : Boolean) : AbstractFile = {
    assert(isDirectory, "not a directory '" + this + "'");
    null;
  }


}
