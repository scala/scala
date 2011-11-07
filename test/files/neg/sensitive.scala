class Certificate{}

object Admin extends Certificate;

class SecurityViolationException extends Exception

object Sensitive {
  def makeSensitive(credentials: Certificate): Sensitive = 
    if (credentials == Admin) new Sensitive() 
    else throw new SecurityViolationException
}
class Sensitive private () {
}

object Attacker {
  val x = Sensitive.makeSensitive(null)
  val y = new Sensitive()
}
  
