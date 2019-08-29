/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.tasty

import java.io.IOException
import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat

import scala.reflect.internal._
import Flags._
import pickling.PickleFormat._
import scala.annotation.switch
import scala.collection.mutable
import scala.reflect.internal.SymbolTable
import scala.util.control.NonFatal

/** @author Martin Odersky
 *  @version 1.0
 */
abstract class UnPickler {
  val symbolTable: SymbolTable
  import symbolTable._

  /** Unpickle symbol table information descending from a class and/or module root
   *  from an array of bytes.
   *  @param bytes      bytearray from which we unpickle
   *  @param offset     offset from which unpickling starts
   *  @param classRoot  the top-level class which is unpickled
   *  @param moduleRoot the top-level module which is unpickled
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle(bytes: Array[Byte], offset: Int, classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String) {
    try {
      new Scan(bytes, offset, classRoot, moduleRoot, filename).run()
    } catch {
      case NonFatal(ex) =>
        /*if (settings.debug.value)*/ ex.printStackTrace()
        throw new RuntimeException("error reading Scala signature of "+filename+": "+ex.getMessage())
    }
  }

  /** Keep track of the symbols pending to be initialized.
    *
    * Useful for reporting on stub errors and cyclic errors.
    */
  private val completingStack = new mutable.ArrayBuffer[Symbol](24)

  class Scan(_bytes: Array[Byte], offset: Int, classRoot: ClassSymbol, moduleRoot: ModuleSymbol, filename: String) {
    //println("unpickle " + classRoot + " and " + moduleRoot)//debug

    protected def debug = settings.debug.value

    private val loadingMirror = mirrorThatLoaded(classRoot)

    /** A map from symbols to their associated `decls` scopes */
    private val symScopes = mutable.HashMap[Symbol, Scope]()

    def run(): Unit = {
      // for all indexes read a symbol
    }

    /** The `decls` scope associated with given symbol */
    protected def symScope(sym: Symbol) = symScopes.getOrElseUpdate(sym, newScope)

    /** Convert to a type error, that is printed gracefully instead of crashing.
     *
     *  Similar in intent to what SymbolLoader does (but here we don't have access to
     *  error reporting, so we rely on the typechecker to report the error).
     */
    def toTypeError(e: MissingRequirementError): TypeError = {
      new TypeError(e.msg)
    }
  }
}
