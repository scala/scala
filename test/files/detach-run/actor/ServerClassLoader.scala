import java.io._
import java.net.{JarURLConnection, URL, URLClassLoader}

import scala.remoting.Debug

  private class ServerObjectInputStream(in: InputStream, cl: ClassLoader)
  extends ObjectInputStream(in) {
    override def resolveClass(cd: ObjectStreamClass): Class[_] = {
      println("[ServerObjectInputStream] resolveClass "+cd.getName)
      try {
        Debug.info("load   class "+cd.getName+" from "+cl)
        val c = cl.loadClass(cd.getName)
        Debug.info("loaded class "+c.getName)
        c
      } catch {
        case cnf: ClassNotFoundException =>
          Debug.info("resolve class (this) "+cd.getName)
          val c = super.resolveClass(cd)
          Debug.info("resolve class (super) "+c.getName)
          c
      }
    }
    override def resolveProxyClass(interfaces: Array[String]): Class[_] = {
      println("[ServerObjectInputStream] resolveProxyClass "+interfaces.toList)
      try {
        val c = cl.loadClass(interfaces.last)
        Debug.info("loaded class "+c.getName)
        c
      } catch {
        case cnf: ClassNotFoundException =>
          Debug.info("resolve proxy class (this) "+interfaces.last)
          val c = super.resolveProxyClass(interfaces)
          Debug.info("resolve proxy class (super) "+c.getName)
          c
      }
    }
  }
/*
  // VARIANT 1
  class ServerClassLoader extends URLClassLoader(urls) {
    import scala.reflect.Manifest
    def load[A](a: Array[Byte])(implicit expected: Manifest[A]): A = {
      val in = new ServerObjectInputStream(new ByteArrayInputStream(a), this)
      val found = in.readObject.asInstanceOf[Manifest[_]]
      if (! (found <:< expected))
        throw new ClassCastException("type mismatch;"+
          "\n found   : "+found+
          "\n required: "+expected)
      val o = in.readObject.asInstanceOf[A]
      in.close()
      o
    }
    override def findClass(name: String): Class[_] = {
      println("[ServerClassLoader] findClass "+name)
      val b = loadClassData(name)
      if (b != null) defineClass(name, b, 0, b.length)
      else super.findClass(name)
    }
    private def loadClassData(name: String): Array[Byte] = {
      println("[ServerClassLoader] loadClassData "+name)
      null
    }
  }
  val serverClassLoader = new ServerClassLoader
*/

/*
  class ServerClassLoader(parent: ClassLoader) extends URLClassLoader(urls, parent) {
    import scala.reflect.Manifest
    def load[A](a: Array[Byte])(implicit expected: Manifest[A]): A = {
      val in = new ServerObjectInputStream(new ByteArrayInputStream(a), this)
      val found = in.readObject.asInstanceOf[Manifest[_]]
      if (! (found <:< expected))
        throw new ClassCastException("type mismatch;"+
          "\n found   : "+found+
          "\n required: "+expected)
      val o = in.readObject.asInstanceOf[A]
      in.close()
      o
    }
    override def findClass(name: String): Class[_] = {
      println("[ServerClassLoader] findClass "+name)
      val b = loadClassData(name)
      if (b != null) defineClass(name, b, 0, b.length)
      else super.findClass(name)
    }
    private def loadClassData(name: String): Array[Byte] = {
      println("[ServerClassLoader] loadClassData "+name)
      null
    }
  }
*/
class ServerClassLoader(urls: Array[URL], parent: ClassLoader)
extends URLClassLoader(urls, parent) {

  private val cache = new collection.mutable.HashMap[String, Class[_]]

  for (url <- urls) {
    val jarurl = new URL("jar:"+url+"!/")
    val con = jarurl.openConnection().asInstanceOf[JarURLConnection]
    val jar = con.getJarFile
    val e = jar.entries
    while (e.hasMoreElements) {
      val ze = e.nextElement
      val path = ze.getName
      if (path endsWith ".class") {
        val size = ze.getSize
        val name = path.replace("/", ".").substring(0, path.length - 6)
        cache += name -> this.loadClass(name)
        println("[ServerClassLoader] added "+name+" ("+size+")")
      }
    };    //jar.close()
  }

  override def findClass(name: String): Class[_] = {
    println("[ServerClassLoader] findClass: name="+name)
    cache get name match {
      case Some(cl) =>
        println(name+" cached"); cl
      case None =>
        println(name+" not cached"); super.findClass(name)
    }
  }

}

/*
try {
    JarFile jarFile = new JarFile(srcPath);
    Enumeration<JarEntry> entries = jarFile.entries();
	String url = "file:" + srcPath;
	System.out.println(url);
	URLClassLoader classLoader = new URLClassLoader(
			new URL[] { new URL(url) });
	while (entries.hasMoreElements()) {
		JarEntry jarEntry = (JarEntry) entries
				.nextElement();
		String classPath = jarEntry.getName();
		if (classPath.endsWith(".class")) {
			String className = classPath.replace("/", ".")
					.substring(0, classPath.length() - 6);
			try {
				Class clazz = classLoader
						.loadClass(className);
				//Et là, tu fais ce que tu vexu avec la classe
			} catch (ClassNotFoundException e1) {
				e1.printStackTrace();
			}
		}
	}
} catch (IOException e1) {
	e1.printStackTrace();
}
*/
