import org.pantsbuild.jarjar
import org.pantsbuild.jarjar._
import org.pantsbuild.jarjar.util._
import scala.collection.JavaConverters._
import java.util.jar._
import java.io._
import sbt._

object JarJar {
  sealed abstract class JarJarConfig {
    def toPatternElement: PatternElement
  }
  object JarJarConfig {
    case class Rule(pattern: String, result: String) extends JarJarConfig {
      def toPatternElement: PatternElement = {
        val rule = new jarjar.Rule
        rule.setPattern(pattern)
        rule.setResult(result)
        rule
      }
    }
    case class Keep(pattern: String) extends JarJarConfig {
      def toPatternElement: PatternElement = {
        val keep = new jarjar.Keep
        keep.setPattern(pattern)
        keep
      }
    }
  }

  sealed abstract class Entry {
    def name: String
    def time: Long
    def data: Array[Byte]
  }

  case class JarEntryInput(jarFile: JarFile, entry: JarEntry) extends Entry {
    def name = entry.getName.replace('\\', '/')
    def time = entry.getTime
    def data = sbt.IO.readBytes(jarFile.getInputStream(entry))
  }
  case class FileInput(base: File, file: File) extends Entry {
    def name = file.relativeTo(base).get.getPath.replace('\\', '/')
    def time = file.lastModified
    def data = sbt.IO.readBytes(file)
  }

  private def newMainProcessor(patterns: java.util.List[PatternElement], verbose: Boolean, skipManifest: Boolean): JarProcessor = {
    val cls = Class.forName("org.pantsbuild.jarjar.MainProcessor")
    val constructor = cls.getConstructor(classOf[java.util.List[_]], java.lang.Boolean.TYPE, java.lang.Boolean.TYPE)
    constructor.setAccessible(true)
    constructor.newInstance(patterns, Boolean.box(verbose), Boolean.box(skipManifest)).asInstanceOf[JarProcessor]
  }

  def apply(in: Iterator[Entry], outdir: File,
            config: Seq[JarJarConfig], verbose: Boolean = false): Seq[File] = {
    val patterns = config.map(_.toPatternElement).asJava
    val processor = newMainProcessor(patterns, verbose, false)
    def process(e: Entry): Option[File] = {
      val struct = new EntryStruct()
      struct.name = e.name
      struct.time = e.time
      struct.data = e.data
      if (processor.process(struct)) {
        if (struct.name.endsWith("/")) None
        else {
          val f = outdir / struct.name
          try {
            f.getParentFile.mkdirs()
            sbt.IO.write(f, struct.data)
          } catch {
            case ex: Exception =>
              throw new IOException(s"Failed to write ${e.name} / ${f.getParentFile} / ${f.getParentFile.exists}", ex)
          }
          Some(f)
        }
      }
      else None
    }
    val processed = in.flatMap(entry => process(entry)).toSet
    val getter = processor.getClass.getDeclaredMethod("getExcludes")
    getter.setAccessible(true)
    val excludes = getter.invoke(processor).asInstanceOf[java.util.Set[String]].asScala
    val excluded = excludes.map { name =>
      val f: File = outdir / name
      if(f.exists && !f.delete())
        throw new IOException("Failed to delete excluded file $f")
      f
    }
    (processed -- excluded).toSeq
  }
}
