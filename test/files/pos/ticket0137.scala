trait AbsM {    
  abstract class MonadCompanion[M[_]] 
  abstract class AbsMonadCompanion extends MonadCompanion[AM] {
    def newTag: Int 
  }

  type AM[_] // to trigger the bug, this must be an abstract type member that comes after the reference to it
}
