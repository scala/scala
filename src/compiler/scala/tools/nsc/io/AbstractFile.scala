package scala.tools.nsc.io;

import java.io.File;
import java.io.IOException;

/*
object AbstractFile {
  def getFile(path : String) : AbstractFile = getFile(new File(path));
	def getFile(file : File  ) : AbstractFile =
    if (file.isFile() && file.exists()) new PlainFile(file);
    else null;

  def getDirectory(path : String) : AbstractFile = getDirectory(new File(path));
  def getDirectory(file : File  ) : AbstractFile =
    if (!file.exists()) null;
    else if (file.isDirectory()) new PlainFile(file);
    else if (file.isFile()) {
      val path = file.getPath();
      if (path.endsWith(".jar") || path.endsWith(".zip"))
        ZipArchive.fromFile(file);
      else null;
    } else null;
}
*/

abstract class AbstractFile {
  /** Returns the name of this abstract file. */
  def name : String;
  /** Returns the path of this abstract file. */
  def path : String;
  /** Returns the underlying File if any and null otherwise. */
  def file : File;
  /** Is this abstract file a directory? */
  def isDirectory : Boolean;
  /** Returns the time that this abstract file was last modified. */
  def lastModified : Long;

  /** Reads the content of this abstract file into a byte array. */
  def read : Array[Byte];

  /** Returns all abstract subfiles of this abstract directory. */
  def list : List[AbstractFile];

  /**
     * Returns the abstract file in this abstract directory with the
     * specified name. If there is no such file, returns null. The
     * argument "directory" tells whether to look for a directory or
     * or a regular file.
     */
  def lookupName(name : String, dir : Boolean) : AbstractFile;

  final override def toString() : String = path;

  /**
  	 * Returns the abstract file in this abstract directory with the
     * specified path relative to it, If there is no such file,
     * returns null. The argument "directory" tells whether to look
     * for a directory or or a regular file.
     */
  def lookupPath(path : String, dir : Boolean) : AbstractFile = {
    val length = path.length();
    val separator = File.separatorChar;
    assert(0 < length && path.lastIndexOf(separator) < length - 1, path);
    var file : AbstractFile = this;
    var start : Int = 0;
    var break = false;
    while (!break) {
		  val index = path.indexOf(separator, start);
      assert(index < 0 || start < index, path+" - "+start+" - "+index);
      val name = path.substring(start, if (index < 0) length else index);
      file = file.lookupName(name, if (index < 0) dir else true);
      if (file == null || index < 0) break = true;
      else start = index + 1;
    }
    file;
  }
}
