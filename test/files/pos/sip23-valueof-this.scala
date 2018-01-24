object Test1 {
  valueOf[this.type]
}

class Test2 {
  valueOf[this.type]
}

class Test3 { self =>
  valueOf[self.type]
}

class A { outerSelf => class B { valueOf[outerSelf.type] } }
