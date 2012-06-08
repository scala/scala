package scala.tools.nsc.interactive.tests.core

import scala.reflect.internal.util.{SourceFile,BatchSourceFile}
import scala.tools.nsc.io.{AbstractFile,Path}

private[tests] object SourcesCollector {
  import Path._
  type SourceFilter =  Path => Boolean

  /**
   * All files below `base` directory that pass the `filter`.
   * With the default `filter` only .scala and .java files are collected.
   * */
  def apply(base: Path, filter: SourceFilter): Array[SourceFile] = {
    assert(base.isDirectory)
    base.walk.filter(filter).map(source).toList.toArray.sortBy(_.file.name)
  }

  private def source(file: Path): SourceFile = source(AbstractFile.getFile(file.toFile))
  private def source(filename: String): SourceFile = source(AbstractFile.getFile(filename))
  private def source(file: AbstractFile): SourceFile = new BatchSourceFile(file)
}