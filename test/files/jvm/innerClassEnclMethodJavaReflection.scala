import scala.reflect.io._
import java.net.URLClassLoader

object Test extends App {
  val jarsOrDirectories = Set("partest.lib", "partest.reflect", "partest.comp") map sys.props

  object AllowedMissingClass {
    // Some classes in scala-compiler.jar have references to jline / ant classes, which seem to be
    // not on the classpath. We just skip over those classes.
    // PENDING: for now we also allow missing $anonfun classes: the optimizer may eliminate some closures
    // that are refferred to in EnclosingClass attributes. SI-9136
    val allowedMissingPackages = Set("scala.tools.jline", "org.apache.tools.ant", "$anonfun")

    def ok(t: Throwable) = {
      allowedMissingPackages.exists(p => t.getMessage.replace('/', '.').contains(p))
    }

    def unapply(t: Throwable): Option[Throwable] = t match {
      case _: NoClassDefFoundError | _: ClassNotFoundException | _: TypeNotPresentException if ok(t) => Some(t)
      case _ => None
    }
  }

  jarsOrDirectories foreach testClasses

  def testClasses(jarOrDirectory: String): Unit = {
    val classPath = AbstractFile.getDirectory(new java.io.File(jarOrDirectory))

    def flatten(f: AbstractFile): Iterator[AbstractFile] =
      if (f.isClassContainer) f.iterator.flatMap(flatten)
      else Iterator(f)

    val classFullNames = flatten(classPath).filter(_.hasExtension("class")).map(_.path.replace("/", ".").replaceAll(".class$", ""))

    // it seems that Class objects can only be GC'd together with their class loader
    //   (http://stackoverflow.com/questions/2433261/when-and-how-are-classes-garbage-collected-in-java)
    // if we just use the same class loader for the entire test (Class.forName), we run out of PermGen
    // even with that, we still neeed a PermGen of 90M or so, the default 64 is not enough. I tried
    // using one class loader per 100 classes, but that didn't help, the classes didn't get GC'd.
    val classLoader = new URLClassLoader(Array(classPath.toURL))

    val faulty = new collection.mutable.ListBuffer[(String, Throwable)]

    def tryGetClass(name: String) = try {
      Some[Class[_]](classLoader.loadClass(name))
    } catch {
      case AllowedMissingClass(_) => None
    }

    for (name <- classFullNames; cls <- tryGetClass(name)) {
      try {
        cls.getEnclosingMethod
        cls.getEnclosingClass
        cls.getEnclosingConstructor
        cls.getDeclaredClasses
      } catch {
        case AllowedMissingClass(_) =>
        case t: Throwable => faulty += ((name, t))
      }
    }

    if (faulty.nonEmpty)
      println(faulty.toList mkString "\n")
  }
}
