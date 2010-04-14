package scala.tools.nsc
package dependencies

import java.io.{InputStream, OutputStream, PrintStream, InputStreamReader, BufferedReader}
import io.{AbstractFile, PlainFile, VirtualFile}

import scala.collection._


trait Files { self : SubComponent =>

  class FileDependencies(val classpath: String) {
    import FileDependencies._

    class Tracker extends mutable.OpenHashMap[AbstractFile, mutable.Set[AbstractFile]] {
      override def default(key: AbstractFile) = {
        this(key) = new mutable.HashSet[AbstractFile]
        this(key)
      }
    }

    val dependencies = new Tracker
    val targets =  new Tracker

    def isEmpty = dependencies.isEmpty && targets.isEmpty

    def emits(source: AbstractFile, result: AbstractFile) =
      targets(source) += result
    def depends(from: AbstractFile, on: AbstractFile) =
      dependencies(from) += on

    def reset(file: AbstractFile) = dependencies -= file

    def cleanEmpty = {
      dependencies foreach {case (_, value) =>
                               value retain (x => x.exists && (x ne removedFile))}
      dependencies retain ((key, value) => key.exists && !value.isEmpty)
      targets foreach {case (_, value) => value retain (_.exists)}
      targets retain ((key, value) => key.exists && !value.isEmpty)
    }

    def containsFile(f: AbstractFile) = targets.contains(f.absolute)

    def invalidatedFiles(maxDepth: Int) = {
      val direct = new mutable.HashSet[AbstractFile]

      for ((file, products) <- targets) {
        // This looks a bit odd. It may seem like one should invalidate a file
        // if *any* of its dependencies are older than it. The forall is there
        // to deal with the fact that a) Some results might have been orphaned
        // and b) Some files might not need changing.
        direct(file) ||= products.forall(d => d.lastModified < file.lastModified)
      }

      val indirect = dependentFiles(maxDepth, direct)

      for ((source, targets) <- targets
           if direct(source) || indirect(source) || (source eq removedFile)) {
        targets foreach (_.delete)
        targets -= source
      }

      (direct, indirect)
    }

    /** Return the set of files that depend on the given changed files.
     *  It computes the transitive closure up to the given depth.
     */
    def dependentFiles(depth: Int, changed: Set[AbstractFile]): Set[AbstractFile] = {
      val indirect = new mutable.HashSet[AbstractFile]
      val newInvalidations = new mutable.HashSet[AbstractFile]

      def invalid(file: AbstractFile) =
        indirect(file) || changed(file) || (file eq removedFile)

      def go(i: Int) : Unit = if(i > 0) {
        newInvalidations.clear
        for((target, depends) <- dependencies if !invalid(target);
            d <- depends)
          newInvalidations(target) ||= invalid(d)

        indirect ++= newInvalidations
        if (!newInvalidations.isEmpty) go(i - 1)
      }

      go(depth)

      indirect --= changed
    }

    def writeTo(file: AbstractFile, fromFile: AbstractFile => String): Unit =
      writeToFile(file)(out => writeTo(new PrintStream(out), fromFile))

    def writeTo(print: PrintStream, fromFile: AbstractFile => String): Unit = {
      def emit(tracker: Tracker) =
        for ((f, ds) <- tracker; d <- ds) print.println(fromFile(f) + arrow + fromFile(d))

      cleanEmpty
      print.println(classpath)
      print.println(separator)
      emit(dependencies)
      print.println(separator)
      emit(targets)
    }
  }

  object FileDependencies {
    private val separator:String = "-------"
    private val arrow = " -> "
    private val removedFile = new VirtualFile("removed")

    private def validLine(l: String) = (l != null) && (l != separator)

    def readFrom(file: AbstractFile, toFile: String => AbstractFile): Option[FileDependencies] =
      readFromFile(file) { in =>
        val reader = new BufferedReader(new InputStreamReader(in))
        val it = new FileDependencies(reader.readLine)

        def readLines(valid: Boolean)(f: (AbstractFile, AbstractFile) => Unit): Boolean = {
          var continue = valid
          var line: String = null
          while (continue && {line = reader.readLine; validLine(line)}) {
            line.split(arrow) match {
              case Array(from, on) => f(toFile(from), toFile(on))
              case _ =>
                global.inform("Parse error: Unrecognised string " + line)
                continue = false
            }
          }
          continue
        }

        reader.readLine

        val dResult = readLines(true)(
          (_, _) match {
            case (null, _)          => // fromFile is removed, it's ok
            case (fromFile, null)   =>
              // onFile is removed, should recompile fromFile
              it.depends(fromFile, removedFile)
            case (fromFile, onFile) => it.depends(fromFile, onFile)
          })

        readLines(dResult)(
          (_, _) match {
            case (null, null)             =>
              // source and target are all removed, it's ok
            case (null, targetFile)       =>
              // source is removed, should remove relative target later
              it.emits(removedFile, targetFile)
            case (_, null)                =>
              // it may has been cleaned outside, or removed during last phase
            case (sourceFile, targetFile) => it.emits(sourceFile, targetFile)
          })

        Some(it)
      }
  }

  def writeToFile[T](file: AbstractFile)(f: OutputStream => T) : T = {
    val out = file.bufferedOutput
    try {
      f(out)
    } finally {
      out.close
    }
  }

  def readFromFile[T](file: AbstractFile)(f: InputStream => T) : T = {
    val in = file.input
    try{
      f(in)
    } finally {
      in.close
    }
  }
}
