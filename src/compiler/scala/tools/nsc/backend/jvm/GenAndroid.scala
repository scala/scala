/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 */

package scala.tools.nsc
package backend.jvm

import ch.epfl.lamp.fjbg._
import symtab.Flags

trait GenAndroid {
  self: GenJVM =>

  import global._
  import icodes._
  import opcodes._

  private val fieldName = "CREATOR"
  private lazy val AndroidParcelableInterface =
    try definitions.getClass("android.os.Parcelable")
    catch { case _: FatalError => NoSymbol }
  private lazy val AndroidCreatorClass =
    if (AndroidParcelableInterface == NoSymbol) NoSymbol
    else definitions.getClass("android.os.Parcelable$Creator")

  def isAndroidParcelableClass(sym: Symbol) = (
    (AndroidParcelableInterface != NoSymbol) &&
    (sym.info.parents contains AndroidParcelableInterface.tpe)
  )

  def addCreatorCode(codegen: BytecodeGenerator, block: BasicBlock) = {
    import codegen._
    val fieldSymbol = clasz.symbol.newValue(NoPosition, newTermName(fieldName))
                        .setFlag(Flags.STATIC | Flags.FINAL)
                        .setInfo(AndroidCreatorClass.tpe)
    val methodSymbol = definitions.getMember(clasz.symbol.companionModule, fieldName)
    clasz addField new IField(fieldSymbol)
    block emit CALL_METHOD(methodSymbol, Static(false))
    block emit STORE_FIELD(fieldSymbol, true)
  }

  def legacyAddCreatorCode(codegen: BytecodeGenerator, clinit: JExtendedCode) = {
    import codegen._
    val creatorType = javaType(AndroidCreatorClass)
    jclass.addNewField(PublicStaticFinal,
                       fieldName,
                       creatorType)
    val moduleName = javaName(clasz.symbol)+"$"
    clinit.emitGETSTATIC(moduleName,
                         nme.MODULE_INSTANCE_FIELD.toString,
                         new JObjectType(moduleName))
    clinit.emitINVOKEVIRTUAL(moduleName, fieldName,
                             new JMethodType(creatorType, Array()))
    clinit.emitPUTSTATIC(jclass.getName(), fieldName, creatorType)
  }
}
