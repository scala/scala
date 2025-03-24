/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.reify

trait States {
  self: Reifier =>

  import global._

  /** Encapsulates reifier state
   *
   *  When untangling reifier symbol tables from the reifier itself,
   *  I discovered that encoding of a symbol table (e.g. producing corresponding reificode)
   *  might cause subsequent reification (e.g. when filling in signatures and annotations for syms).
   *
   *  This is a mess in the face of nested reifications, splices and inlining of thereof,
   *  so I made `SymbolTable` immutable, which brought a significant amount of confidence.
   *
   *  However that wasn't enough. Sure, symbol table became immutable, but the reifier still needed
   *  to mutate its `symtab` field during reification. This caused nasty desyncs between the table being encoded
   *  and the table of the underlying reifier, so I decided to encapsulate the entire state here,
   *  so that encoding can backup the state before it starts and restore it after it completes.
   */
  val state = new State

  // todo. rewrite the reifier so that we don't need mutable state anymore
  // to aid you with that I've already removed all the setters from the reifier
  // so all the places that involve mutations are forced to do that by explicitly mentioning `state`
  class State {
    var symtab = SymbolTable()
    var reifyTreeSymbols = false
    var reifyTreeTypes = false
    private var _reificationIsConcrete = true
    def reificationIsConcrete: Boolean = _reificationIsConcrete
    def reificationIsConcrete_=(value: Boolean): Unit = {
      _reificationIsConcrete = value
      if (!value && concrete) {
        current match {
          case tpe: Type => CannotReifyWeakType(s" having unresolved type parameter $tpe")
          case sym: Symbol => CannotReifyWeakType(s" referring to ${sym.kindString} ${sym.fullName} local to the reifee")
          case _ => CannotReifyWeakType("")
        }
      }
    }
    var reifyStack = reifee :: Nil
    var localSymbols = Map[Symbol, Int]()

    def backup: State = {
      val backup = new State
      backup.symtab = this.symtab
      backup.reifyTreeSymbols = this.reifyTreeSymbols
      backup.reifyTreeTypes = this.reifyTreeTypes
      backup._reificationIsConcrete = this._reificationIsConcrete
      backup.reifyStack = this.reifyStack
      backup.localSymbols = this.localSymbols
      backup
    }

    def restore(backup: State): Unit = {
      this.symtab = backup.symtab
      this.reifyTreeSymbols = backup.reifyTreeSymbols
      this.reifyTreeTypes = backup.reifyTreeTypes
      this._reificationIsConcrete = backup._reificationIsConcrete
      this.reifyStack = backup.reifyStack
      this.localSymbols = backup.localSymbols
    }
  }
}
