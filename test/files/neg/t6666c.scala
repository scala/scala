class C(a: Any)
class D extends C({def x = 0; object X { x }})

class C1(a: () => Any)
class D1 extends C1({def x = 0; () => {object X { x }}})

class C2(a: => Any)
class D2 extends C2({def x = 0; object X { x }})
