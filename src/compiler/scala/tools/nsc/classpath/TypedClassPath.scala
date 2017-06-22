package scala.tools.nsc.classpath

import scala.reflect.io.AbstractFile

sealed trait TypedClassPath[FileEntryType <: SingleClassRepresentation] extends BasicClassPath {
  protected def isValidFilename(fileName:String):Boolean
  protected def toRepr(abstractFile: AbstractFile):FileEntryType

}

trait BasicNoSourcesClassPath extends TypedClassPath[ClassFileEntry] with NoSourcePaths {
  override protected final def isValidFilename(fileName: String): Boolean = fileName.endsWith(".class")
  override protected final def toRepr(abstractFile: AbstractFile): ClassFileEntryImpl = ClassFileEntryImpl(abstractFile)
}

trait BasicNoClassesClassPath extends TypedClassPath[SourceFileEntry] with NoClassPaths{
  override protected final def isValidFilename(fileName: String): Boolean =
    fileName.endsWith(".java") || fileName.endsWith(".scala")

  override protected final def toRepr(abstractFile: AbstractFile): SourceFileEntryImpl = SourceFileEntryImpl(abstractFile)

}

trait CommonNoSourcesClassPath extends BasicNoSourcesClassPath {
  self : CommonClassPath[ClassFileEntry] =>

  override private[nsc] def classes(inPackage: String) = files(inPackage)

  override def findClassFile(className: String): Option[AbstractFile] = {
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)
    content.data(pkg).filesByName.get(simpleClassName)
  }



}
trait CommonNoClassesClassPath extends BasicNoClassesClassPath {
  self : CommonClassPath[SourceFileEntry] =>
  override private[nsc] def sources(inPackage: String) = files(inPackage)

  /** The whole sourcepath in the form of one String.
    */
  override def asSourcePathString: String = asClassPathString
}
