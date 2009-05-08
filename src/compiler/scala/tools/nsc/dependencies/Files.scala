package scala.tools.nsc.dependencies;

import java.io.{File => JFile, _}

import scala.collection.mutable._;

trait Files{

  implicit def toFile(name : String) : File = toFile(new JFile(name));
  implicit def toFile(jf : JFile) : File = new File(jf);

  class FileDependencies(val classpath : String){
    class Tracker extends OpenHashMap[File, Set[File]]{
      override def default(key : File) = {
        this(key) = new HashSet[File];
        this(key);
      }
    }

    val dependencies = new Tracker
    val targets =  new Tracker;

    def isEmpty = dependencies.isEmpty && targets.isEmpty

    def emits(source : File, result : File) = targets(source.absolute) += result.absolute;
    def depends(from : File, on : File) = dependencies(from.absolute) += on.absolute;

    def reset(file : File) = dependencies -= file;

    def cleanEmpty() = {
      dependencies.foreach({case (key, value) => value.retain(_.exists)})
      dependencies.retain((key, value) => key.exists && !value.isEmpty)
    }

    def containsFile(f : File) = targets.contains(f.absolute)

    def invalidatedFiles(maxDepth : Int) = {
      val direct = new HashSet[File];

      for ((file, products) <- targets) {
        // This looks a bit odd. It may seem like one should invalidate a file
        // if *any* of its dependencies are older than it. The forall is there
        // to deal with the fact that a) Some results might have been orphaned
        // and b) Some files might not need changing.
        direct(file) ||= products.forall(d => d.lastModified < file.lastModified)
      }


      val seen = new HashSet[File];
      val indirect = new HashSet[File];
      val newInvalidations = new HashSet[File];

      def invalid(file : File) = indirect(file) || direct(file);

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

      go(maxDepth)

      indirect --= direct

      for ((source, targets) <- targets; if (invalid(source))){
        targets.foreach(_.rm);
        targets -= source;
      }

      (direct, indirect);
    }

    def writeTo(file : File) : Unit = file.writeTo(out => writeTo(new PrintStream(out)));
    def writeTo(print : PrintStream) : Unit = {
      cleanEmpty();
      def emit(tracker : Tracker){
        for ((f, ds) <- tracker;
              d <- ds){
          print.println(f + " -> " + d);
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

    def readFrom(file : File) = file.readFrom(in => {
      val reader = new BufferedReader(new InputStreamReader(in));
      val it = new FileDependencies(reader.readLine);
      reader.readLine;
      var line : String = null;
      while ({line = reader.readLine; (line != null) && (line != Separator)}){
        line.split(" -> ") match {
          case Array(from, on) => it.depends(from, on);
          case x => error("Parse error: Unrecognised string " + line);
        };
      }

      while ({line = reader.readLine; (line != null) && (line != Separator)}){
        line.split(" -> ") match {
          case Array(source, target) => it.emits(source, target);
          case x => error("Parse error: Unrecognised string " + line);
        };
      }

      it;
    })
  }


  def currentDirectory = new File(new JFile("."))

  case class File private[Files](val underlying : JFile){
    if (underlying == null) throw new NullPointerException();

    def absolute : File = underlying.getAbsoluteFile;
    def canonical : File = underlying.getCanonicalFile

    def assertDirectory =
      if (exists && !isDirectory) error(this + " is not a directory")
      else this;

    def assertExists =
      if (!exists) error(this + " does not exist")
      else this;

    def lastModified = underlying.lastModified

    def list : Iterable[File] =
      assertExists.assertDirectory.underlying.listFiles.view.map(toFile)

    def / (file : File) : File =
      new JFile(assertDirectory.toString,
                file.toString)

    override def toString = {
      val it = underlying.getPath;
      if (it.length == 0) "."
      else it
    }

    def exists = underlying.exists
    def isDirectory = underlying.isDirectory;

    def parent : File = {
      val x = underlying.getParentFile;
      if (x == null) currentDirectory
      else x
    }

    def create : Boolean = {parent.mkdir; underlying.createNewFile }
    def mkdir : Boolean =
      if (exists) { assertDirectory; false }
      else {parent.mkdir; underlying.mkdir; }

    def rm : Boolean = {
      if (isDirectory) list.foreach(_.rm);
      underlying.delete;
    }

    def descendants : Iterable[File] =
      list.flatMap(x => if (x.isDirectory) x.descendants else List(x))

    def extension = {
      val name = toString;
      val i = name.lastIndexOf('.');
      if (i == -1) "" else name.substring(i + 1)
    }

    def writeTo[T](f : OutputStream => T) : T = {
      val out = new FileOutputStream(underlying);
      try {
        f(out);
      } finally {
        out.close;
      }
    }

    def readFrom[T](f : InputStream => T) : T = {
      val in = new FileInputStream(underlying);
      try{
        f(in);
      } finally {
        in.close;
      }
    }
  }

}

object Files extends Files;
