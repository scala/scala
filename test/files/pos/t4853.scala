object Animal {
  def main(args: Array[String]) { new Animal[Awake].goToSleep }
}

class Animal[A <: AwakeOrAsleep] {
  def goToSleep[B >: A <: Awake]: Animal[Asleep] = new Animal[Asleep] 
  def wakeUp[B >: A <: Asleep]: Animal[Awake] = new Animal[Awake]
}

sealed trait AwakeOrAsleep
trait Awake extends AwakeOrAsleep
trait Asleep extends AwakeOrAsleep
