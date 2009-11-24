package scala.tools.nsc
package dependencies;

import java.io.{InputStream, OutputStream, PrintStream, InputStreamReader, BufferedReader}
import io.{AbstractFile, PlainFile}

import scala.collection._;import scala.tools.nsc.io.VirtualFile


trait Files { self : SubComponent =>

  class FileDependencies(val classpath : String) {
    import FileDependencies._

    class Tracker extends mutable.OpenHashMap[AbstractFile, mutable.Set[AbstractFile]]{
      override def default(key: AbstractFile) = {
        this(key) = new mutable.HashSet[AbstractFile];
        this(key);
      }
    }

    val dependencies = new Tracker
    val targets =  new Tracker;

    def isEmpty = dependencies.isEmpty && targets.isEmpty

    def emits(source: AbstractFile, result: AbstractFile) =
      targets(source) += result;
    def depends(from: AbstractFile, on: AbstractFile) =
      dependencies(from) += on;

    def reset(file: AbstractFile) = dependencies -= file;

    def cleanEmpty() = {
      dependencies foreach {case (key, value) => value.retain(x => x.exists && (x ne RemovedFile))}
      dependencies.retain((key, value) => key.exists && !value.isEmpty)
      targets foreach {case (key, value) => value.retain(_.exists)}
      targets.retain((key, value) => key.exists && !value.isEmpty)
    }

    def containsFile(f: AbstractFile) = targets.contains(f.absolute)

    def invalidatedFiles(maxDepth : Int) = {
      val direct = new mutable.HashSet[AbstractFile];

      for ((file, products) <- targets) {
        // This looks a bit odd. It may seem like one should invalidate a file
        // if *any* of its dependencies are older than it. The forall is there
        // to deal with the fact that a) Some results might have been orphaned
        // and b) Some files might not need changing.
        direct(file) ||= products.forall(d => d.lastModified < file.lastModified)
      }

      val indirect = dependentFiles(maxDepth, direct)

      for ((source, targets) <- targets;
           if direct(source) || indirect(source) || (source eq RemovedFile)){
        targets.foreach(_.delete);
        targets -= source;
      }

      (direct, indirect);
    }

    /** Return the set of files that depend on the given changed files.
     *  It computes the transitive closure up to the given depth.
     */
    def dependentFiles(depth: Int, changed: Set[AbstractFile]): Set[AbstractFile] = {
      val indirect = new mutable.HashSet[AbstractFile];
      val newInvalidations = new mutable.HashSet[AbstractFile];

      def invalid(file: AbstractFile) = indirect(file) || changed(file) || (file eq RemovedFile)

      def go(i : Int) : Unit = if(i > 0){
        newInvalidations.clear;
        for((target, depends) <- dependencies;
            if !invalid(target);
            d <- depends){
          newInvalidations(target) ||= invalid(d)
        }
        indirect ++= newInvalidations;
        if(!newInvalidations.isEmpty) go(i - 1);
        else ()
      }

      go(depth)

      indirect --= changed
    }

    def writeTo(file: AbstractFile, fromFile : AbstractFile => String) {
      writeToFile(file)(out => writeTo(new PrintStream(out), fromFile))
    }

    def writeTo(print : PrintStream, fromFile : AbstractFile => String) : Unit = {
      cleanEmpty();
      def emit(tracker : Tracker){
        for ((f, ds) <- tracker;
              d <- ds){
          print.println(fromFile(f) + " -> " + fromFile(d));
        }
      }

      print.println(classpath);
      print.println(FileDependencies.Separator)
      emit(dependencies);
      print.println(FileDependencies.Separator)
      emit(targets);

    }
  }

  object FileDependencies{
    val Separator = "-------";
    private val RemovedFile = new VirtualFile("removed")

    def readFrom(file: AbstractFile, toFile : String => AbstractFile): Option[FileDependencies] = readFromFile(file) { in =>
      val reader = new BufferedReader(new InputStreamReader(in))
      val it = new FileDependencies(reader.readLine)
      reader.readLine
      var line : String = null
      while ({line = reader.readLine; (line != null) && (line != Separator)}){
        line.split(" -> ") match {
          case Array(from, on) =>
            (toFile(from), toFile(on)) match {
              case (null, _) => // fromFile is removed, it's ok
              case (fromFile, null) => it.depends(fromFile, RemovedFile) // onFile is removed, should recompile fromFile
              case (fromFile, onFile) => it.depends(fromFile, onFile)
            }
          case x => global.inform("Parse error: Unrecognised string " + line); return None
        }
      }

      while ({line = reader.readLine; (line != null) && (line != Separator)}){
        line.split(" -> ") match {
          case Array(source, target) =>
            val targetFile = toFile(target)
            (toFile(source), toFile(target)) match {
              case (null, null) => // source and target are all removed, it's ok
              case (null, targetFile) => it.emits(RemovedFile, targetFile) // source is removed, should remove relative target later
              case (_, null) => // it may has been cleaned outside, or removed during last phase
              case (sourceFile, targetFile) => it.emits(sourceFile, targetFile)
            }
          case x => global.inform("Parse error: Unrecognised string " + line); return None
        }
      }

      Some(it)
    }
  }

  def writeToFile[T](file: AbstractFile)(f: OutputStream => T) : T = {
    val out = file.output
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
