package scala.reflect
package api

trait Importers { self: Universe =>

  def mkImporter(from0: Universe): Importer { val from: from0.type }

  trait Importer {
    val from: Universe

    val reverse: from.Importer { val from: self.type }

    def importSymbol(sym: from.Symbol): Symbol

    def importType(tpe: from.Type): Type

    def importTree(tree: from.Tree): Tree

    def importPosition(pos: from.Position): Position
  }
}