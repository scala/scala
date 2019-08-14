package scala.tools.nsc

import java.io.OutputStreamWriter
import java.nio.charset.Charset
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}
import scala.collection.JavaConverters._
import javax.tools.ToolProvider

import scala.reflect.internal.util.SourceFile
import scala.reflect.io.AbstractFile
import scala.tools.nsc.FileUtils._
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.BatchSourceFile

object DeterminismTester extends DeterminismTester {
  def main(args: Array[String]): Unit = {
    val (scalacOpts, sourceFilesPaths) = args.indexOf("--") match {
      case -1 => (Nil, args.toList)
      case i =>
        val tuple = args.toList.splitAt(i)
        (tuple._1, tuple._2.drop(1))
    }
    def isJavaOrScala(p: Path) = {
      val name = p.getFileName.toString
      name.endsWith(".java") || name.endsWith(".scala")
    }
    def expand(path: Path): Seq[Path] = {
      if (Files.isDirectory(path))
        Files.walk(path).iterator().asScala.filter(isJavaOrScala).toList
      else path :: Nil
    }
    val sourceFiles = sourceFilesPaths.map(Paths.get(_)).flatMap(expand).map(path => new BatchSourceFile(AbstractFile.getFile(path.toFile)))
    test(scalacOpts, sourceFiles :: Nil)
  }
}

class DeterminismTester {

  def test(groups: List[List[SourceFile]]): Unit = test(Nil, groups)
  def test(scalacOptions: List[String], groups: List[List[SourceFile]]): Unit = {
    val referenceOutput = Files.createTempDirectory("reference")

    def compile(output: Path, files: List[SourceFile]): Unit = {
      // println("compile: " + files)
      val g = new Global(new Settings)
      g.settings.usejavacp.value = true
      g.settings.classpath.value = output.toAbsolutePath.toString
      g.settings.outputDirs.setSingleOutput(output.toString)
      g.settings.async.value = true
      g.settings.processArguments(scalacOptions, true)
      val storeReporter = new StoreReporter(g.settings)
      g.reporter = storeReporter
      import g._
      val r = new Run
      // println("scalac " + files.mkString(" "))
      r.compileSources(files)
      Predef.assert(!storeReporter.hasErrors, storeReporter.infos.mkString("\n"))
      files.filter(_.file.name.endsWith(".java")) match {
        case Nil =>
        case javaSources =>
          def tempFileFor(s: SourceFile): Path = {
            val f = output.resolve(s.file.name)
            Files.write(f, new String(s.content).getBytes(Charset.defaultCharset()))
          }
          val options = List("-d", output.toString)
          val javac = ToolProvider.getSystemJavaCompiler
          assert(javac != null, "No javac from getSystemJavaCompiler. If the java on your path isn't a JDK version, but $JAVA_HOME is, launch sbt with --java-home \"$JAVA_HOME\"")
          val fileMan = javac.getStandardFileManager(null, null, null)
          val javaFileObjects = fileMan.getJavaFileObjects(javaSources.map(s => tempFileFor(s).toAbsolutePath.toString): _*)
          val task = javac.getTask(new OutputStreamWriter(System.out), fileMan, null, options.asJava, Nil.asJava, javaFileObjects)
          val result = task.call()
          Predef.assert(result)
      }
    }

    for (group <- groups.init) {
      compile(referenceOutput, group)
    }
    compile(referenceOutput, groups.last)

    class CopyVisitor(src: Path, dest: Path) extends SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.createDirectories(dest.resolve(src.relativize(dir)))
        super.preVisitDirectory(dir, attrs)
      }
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.copy(file, dest.resolve(src.relativize(file)))
        super.visitFile(file, attrs)
      }
    }
    val permutations: List[List[SourceFile]] = if (groups.last.size > 32) {
      groups.last.reverse :: groups.last.map(_ :: Nil)
    } else permutationsWithSubsets(groups.last)
    for (permutation <- permutations) {
      val recompileOutput = Files.createTempDirectory("recompileOutput")
      copyRecursive(referenceOutput, recompileOutput)
      compile(recompileOutput, permutation)
      assertDirectorySame(referenceOutput, recompileOutput, permutation.toString)
      deleteRecursive(recompileOutput)
    }
    deleteRecursive(referenceOutput)

  }
  def permutationsWithSubsets[A](as: List[A]): List[List[A]] =
    as.permutations.toList.flatMap(_.inits.filter(_.nonEmpty)).distinct

}
