/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac;

import scalac.util.*;
import scalac.symtab.NameMangler;
import scalac.ast.parser.Sourcefile;
import scalac.ast.Tree;
import scala.compiler.typechecker.*;
import java.io.*;
import java.util.*;


/** A representation for a compilation unit in scala
 *
 *  @author     Matthias Zenger
 *  @version    1.0
 */
public class Unit {

    /** the global compilation environment
     */
    public Global global;

    /** the associated source code file
     */
    public Sourcefile source;

    /** the content of the compilation unit in tree form
     */
    public Tree[] body;

    /** the generated symbol data; Symbol -> byte[]
    public SymData symdata;
     */

    /** the name mangler
     */
    public NameMangler mangler = new NameMangler();

    /** number of errors issued for this compilation unit
     */
    public int errors;

    /** number of warnings issued for this compilation unit
     */
    public int warnings;

    /** number of notes issued for this compilation unit
     */
    public int notes;

    public Unit(Global global, Sourcefile source) {
        this.global = global;
        this.source = source;
    }
    /*
    public void print(String message) {
        print(System.out, message);
    }

    public void print(PrintStream out, String message) {
        out.println("[[begin " + message + "]]");
        new Printer(out).printCompilationUnit(this);
        out.println();
        out.println("[[end " + message + "]]");
    }
    */
    /** issue an error in this compilation unit
     */
    public void error(String message) {
        error(Position.NOPOS, message);
    }

    /** issue an error in this compilation unit at a specific location
     */
    public void error(int pos, String message) {
        boolean hidden = source.testAndSetLog(pos, message);
        if (!hidden) errors++;
        global.reporter.error(source.getMessage(pos, message), hidden);
    }

    /** issue a warning in this compilation unit
     */
    public void warning(String message) {
        warning(Position.NOPOS, message);
    }

    /** issue a warning in this compilation unit at a specific location
     */
    public void warning(int pos, String message) {
        if (global.reporter.nowarn) return;
        message = "warning: " + message;
        boolean hidden = source.testAndSetLog(pos, message);
        if (!hidden) warnings++;
        global.reporter.warning(source.getMessage(pos, message), hidden);
    }

    /** issue a note in this compilation unit
     */
    public void note(String message) {
        note(Position.NOPOS, message);
    }

    /** issue a note in this compilation unit at a specific location
     */
    public void note(int pos, String message) {
        global.reporter.note(source.getMessage(pos, message));
        notes++;
    }

    /** return a string representation
     */
    public String toString() {
        return source.toString();
    }
}
