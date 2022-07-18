
interface CyclicTypeContainer {
    CyclicType.CyclicTypeChild method1();
    String method2();
}

interface CyclicType extends ParametrizedInterface<CyclicType.CyclicTypeChild> {
    interface CyclicTypeChild {
    }
}

interface ParametrizedInterface<A> {
}
