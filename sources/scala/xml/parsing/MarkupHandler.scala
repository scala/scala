package scala.xml.parsing;

import scala.collection.immutable.Map ;
import scala.collection.mutable ;

/** @todo: make ConstructingMarkupHandler */
abstract class MarkupHandler[MarkupType, AVType] {

  def attributeCDataValue(pos: int, str:String): AttribValue[AVType];
  def attributeEmbedded(pos: int, x:MarkupType): AttribValue[AVType];
  def element(pos: int, label: String, attrMap1: mutable.Map[String,AttribValue[AVType]], args: mutable.Buffer[MarkupType]): MarkupType;

  def charData(pos: Int, charData: scala.xml.CharData ): MarkupType;
  def procInstr(pos: Int, procInstr: scala.xml.ProcInstr ): MarkupType;
  def comment(pos: Int, comment: scala.xml.Comment ): MarkupType;
  def entityRef(pos: Int, n: String): MarkupType;

  def text(pos: Int, txt:String): MarkupType;

}
