trait coll[+m[+x]]

class FooInvar[x]                 
class FooContra[-x]
class FooCov[+x]

object test {
  var ok: coll[FooCov] = _
   
  var x: coll[FooInvar] = _    // TODO: error should be reported only once instead of separately for getter and setter
  var y: coll[FooContra] = _
}
