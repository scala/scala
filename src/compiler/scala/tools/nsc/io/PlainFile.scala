package scala.tools.nsc.io;

import java.io._;

object PlainFile {
  def fromPath(path : String) : AbstractFile = fromFile(new File(path));

  def fromFile(file : File) : AbstractFile =
    if (file.exists()) new PlainFile(file) else null;

}

class PlainFile(val file : File) extends AbstractFile {
  assert(file != null);
  if (!file.exists()) throw new Error("non-existent file: " + file);

  def name = file.getName();
  def path = file.getPath();
  def isDirectory = file.isDirectory();
  def lastModified = file.lastModified();

  def read = {
    assert(!isDirectory, "cannot read directory");
    val in = new FileInputStream(file);
    var rest = file.length().asInstanceOf[Int];
    val buf = new Array[Byte](rest);
    var break = false;
    while (!break) {
      val res = in.read(buf, buf.length - rest, rest);
      if (res == -1)
        throw new IOException("read error");
      rest = rest - res;
      break = !(rest > 0);
    }
    in.close();
    buf;
  }

  def lookupName0(name : String) : AbstractFile = {
    assert(isDirectory, "not a directory '" + this + "'");
    val child = new File(file, name);
    if (!child.exists()) null;
    else new PlainFile(child);
  }

  def lookupName(name : String, isDirectory: Boolean) : AbstractFile = {
    val result = lookupName0(name);
    if (result == null) null;
    else if (isDirectory != result.isDirectory) null;
    else result;
  }

  def list : List[AbstractFile] = {
    assert(isDirectory, "not a directory '" + this + "'");
    val names = file.list();
    if (names == null || names.length == 0) return Nil;

    val ret = for (val name <- names.toList) yield lookupName0(name);

    for (val file <- ret; file != null) yield file;
  }
}
