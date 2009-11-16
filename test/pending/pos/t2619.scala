abstract class F {
    final def apply(x: Int): AnyRef = null
}
abstract class AbstractModule {
    def as: List[AnyRef]
    def ms: List[AbstractModule]
    def fs: List[F] = Nil
    def rs(x: Int): List[AnyRef] = fs.map(_(x))
}
abstract class ModuleType1 extends AbstractModule {}
abstract class ModuleType2 extends AbstractModule {}

object ModuleAE extends ModuleType1 {
    def as = Nil
    def ms = Nil
}
object ModuleAF extends ModuleType2 {
    def as = Nil
    def ms = List(ModuleAE)
}
object ModuleAG extends ModuleType1 {
    def as = List("")
    def ms = Nil
}
object ModuleAI extends ModuleType1 {
    def as = Nil
    def ms = List(ModuleAE)
}
object ModuleAK extends ModuleType2 {
    def as = Nil
    def ms = List(ModuleAF)
}
object ModuleAL extends ModuleType1 {
    def as = Nil
    def ms = List(
        ModuleAG,
        ModuleAI
    )
}
object ModuleAM extends ModuleType1 {
    def as = Nil
    def ms = List(
        ModuleAL,
        ModuleAE
    ) ::: List(ModuleAK)
}
object ModuleBE extends ModuleType1 {
    def as = Nil
    def ms = Nil
}
object ModuleBF extends ModuleType2 {
    def as = Nil
    def ms = List(ModuleBE)
}
object ModuleBG extends ModuleType1 {
    def as = List("")
    def ms = Nil
}
object ModuleBI extends ModuleType1 {
    def as = Nil
    def ms = List(ModuleBE)
}
object ModuleBK extends ModuleType2 {
    def as = Nil
    def ms = List(ModuleBF)
}
object ModuleBL extends ModuleType1 {
    def as = Nil
    def ms = List(
        ModuleBG,
        ModuleBI
    )
}
object ModuleBM extends ModuleType1 {
    def as = Nil
    def ms = List(
        ModuleBL,
        ModuleBE
    ) ::: List(ModuleBK)
}