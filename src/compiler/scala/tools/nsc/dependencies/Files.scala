package scala.tools.nsc
package dependencies;

import java.io.{InputStream, OutputStream, PrintStream, InputStreamReader, BufferedReader}
import io.{AbstractFile, PlainFile}

import scala.collection._;

trait Files { self : SubComponent =>

  class FileDependencies(val classpath : String) {

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
      dependencies.foreach({case (key, value) => value.retain(_.exists)})
      dependencies.retain((key, value) => key.exists && !value.isEmpty)
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
           if direct(source) || indirect(source)){
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

      def invalid(file: AbstractFile) = indirect(file) || changed(file);

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

    def readFrom(file: AbstractFile, toFile : String => AbstractFile): Option[FileDependencies] = readFromFile(file) { in =>
      val reader = new BufferedReader(new InputStreamReader(in))
      val it = new FileDependencies(reader.readLine)
      reader.readLine
      var line : String = null
      while ({line = reader.readLine; (line != null) && (line != Separator)}){
        line.split(" -> ") match {
          case Array(from, on) => it.depends(toFile(from), toFile(on));
          case x => global.inform("Parse error: Unrecognised string " + line); return None
        }
      }

      while ({line = reader.readLine; (line != null) && (line != Separator)}){
        line.split(" -> ") match {
          case Array(source, target) => it.emits(toFile(source), toFile(target));
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
