package scala.tools.nsc

import java.io.{File, FileOutputStream, IOException}
import java.util.jar.{JarFile, JarEntry, JarOutputStream, Attributes}
import scala.collection.convert.WrapAsScala
import WrapAsScala.{enumerationAsScalaIterator, mapAsScalaMap}

class JarLister {

  def process(args: Array[String]): Boolean = {
    if (args.length == 0 || args(0) == "-help") {
      Console.println("""Usage: jarlister [<options>] <jar name>
   or  jarlister -help

Options:

 -o <file>  specify output jar file
""")
      return true
    }
    if (args(0) == "-o") process(args(2), args(1)) else process(args(0), "")
  }

  def process(name: String, output: String): Boolean = {
    val listedName = if (output == "") name + ".lst" else output
    val file = new File(name);
    val jarFile = new JarFile(file);
    val listedFile = new File(listedName);

    val manifest = jarFile.getManifest
    val mfEntries = manifest.getEntries

    for (entry <- jarFile.entries) if (entry.getName.endsWith(".class") && !mfEntries.contains(entry.getName)) {
      mfEntries(entry.getName) = new Attributes
    }

    val fos = new FileOutputStream(listedFile)
    val jos = new JarOutputStream(fos, manifest)

    for (entry <- jarFile.entries) if (!entry.getName.equalsIgnoreCase(JarFile.MANIFEST_NAME)) {
      jos.putNextEntry(entry)
      writeBytes(jarFile, entry, jos)
    }

    jos.finish
    fos.close
    jarFile.close

    if (output == "") {
      if (!listedFile.renameTo(file)) {
        val orig = new File(name + ".orig");
        if (file.renameTo(orig)) {
          if (listedFile.renameTo(file)) orig.delete
          else return false
        } else return false
      }
    }
    return true
  }

  val buffer = new Array[Byte](8192)
  def writeBytes(f: JarFile, e: JarEntry, os: JarOutputStream) {
    val is = f.getInputStream(e)
    var left = e.getSize
    var n = 0
    while(left > 0) {
      n = is.read(buffer, 0, buffer.length)
      if (n == -1) throw new IOException("read error")
      os.write(buffer, 0, n)
      left -= n
    }
  }
}

object JarLister extends JarLister {
  def main(args: Array[String]) {
    if (!process(args))
      sys.exit(1)
  }
}
