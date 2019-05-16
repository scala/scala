package scala.reflect

import scala.reflect.internal.Names
import scala.reflect.internal.names.{NameBase, NameTable}

class ExistingSynchronizedNameTable extends ExistingNameTable{
  override def synchronizeNames: Boolean = true
}
class ExistingNameTable extends NameTable[AnyRef] with Names{
  override def size: Int = -1

  override def find(source: String) = newTermName(source.toCharArray(), 0, source.length(), null)


  override def find(chars: Array[Char], start: Int, count: Int) = newTermName(chars, start, count)

}
