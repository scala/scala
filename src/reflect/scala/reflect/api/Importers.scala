package scala.reflect
package api

// [Martin] Importers need to be made mirror aware.
// [Eugene++] this is important
trait Importers { self: Universe =>

  def mkImporter(from0: Universe): Importer { val from: from0.type }

  trait Importer {
    val from: Universe

    val reverse: from.Importer { val from: self.type }

    def importSymbol(sym: from.Symbol): Symbol

    def importType(tpe: from.Type): Type

    def importTree(tree: from.Tree): Tree
  }
}