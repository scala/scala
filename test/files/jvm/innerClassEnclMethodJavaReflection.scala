import scala.reflect.io._

object Test extends App {
  val jars = Set("scala-library.jar", "scala-reflect.jar", "scala-compiler.jar")

  // Some classes in scala-compiler.jar have references to jline classes, which seem to be not on the classpath.
  // We just skip over those classes that depend on jline.
  val allowedMissingPackages = Set("jline")

  object AllowedMissingClass {
    def unapply(t: Throwable): Option[Throwable] = t match {
      case e: NoClassDefFoundError if allowedMissingPackages.exists(p => e.getMessage.startsWith(p)) => Some(t)
      case _ => None
    }
  }

  val jarFileNames = ClassLoader.getSystemClassLoader match {
    case l: java.net.URLClassLoader =>
      l.getURLs.toList.filter(u => jars.exists(j => u.toString.contains(j))).map(_.getFile)
    case _ =>
      Nil
  }

  jarFileNames foreach testJar


  def testJar(fileName: String): Unit = {
    val jarFile = AbstractFile.getDirectory(new java.io.File(fileName))

    def flatten(f: AbstractFile): Iterator[AbstractFile] =
      if (f.isClassContainer) f.iterator.flatMap(flatten)
      else Iterator(f)

    val classFullNames = flatten(jarFile).filter(_.hasExtension("class")).map(_.path.replace("/", ".").replaceAll(".class$", ""))

    val classes = classFullNames.flatMap(name => {
      try {
        Iterator[Class[_]](Class.forName(name))
      } catch {
        case AllowedMissingClass(_) => Iterator.empty
      }
    })

    val faulty = classes.map(cls => (cls, 
      try {
        Left(List(
          cls.getEnclosingMethod,
          cls.getEnclosingClass,
          cls.getEnclosingConstructor,
          cls.getDeclaredClasses))
      } catch {
        case AllowedMissingClass(_) => Left(Nil)
        case t: Throwable => Right(t)
      })).filter(_._2.isRight)

    if (faulty.nonEmpty)
      println(faulty mkString "\n")
  }
}
