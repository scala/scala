/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc;

import scala.tools.nsc.util.{SourceFile, Position};
import scala.tools.nsc.util.FreshNameCreator;
import scala.tools.nsc.io.AbstractFile;
import scala.collection.mutable.HashSet;

trait CompilationUnits requires Global {

  class CompilationUnit(val source: SourceFile) {

    /** the fresh name creator */
    val fresh = new FreshNameCreator;

    /** the content of the compilation unit in tree form */
    var body: Tree = EmptyTree;

    val depends = new HashSet[AbstractFile];

    def position(pos: int) = new Position(source, pos);

    val errorPositions = new HashSet[int]

    def error(pos: int, msg: String) = {
      if (!(errorPositions contains pos)) {
        errorPositions += pos;
        reporter.error(position(pos), msg);
      }
    }
    def warning(pos: int, msg: String) = reporter.warning(position(pos), msg);
    override def toString() = source.toString();

  }
}


