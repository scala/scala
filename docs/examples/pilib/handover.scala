package examples.pilib

/**
 * Handover example with recursive types for channels.
 */
object handoverRecursive {

  import concurrent.pilib._

  val random = new java.util.Random()

  /**
   * Recursive type for channels that carry a channel "unit" and
   * an object of the type we define.
   */
  class Switch extends Chan[Pair[Chan[unit], Switch]]

  /**
   * Car.
   */
  def Car(talk: Chan[unit], switch: Switch): unit =
    choice (
      switch * ({ case Pair(t,s) => Car(t, s) }),
      talk(()) * ( {
        Thread.sleep(1 + random.nextInt(1000));
        System.out.println("Car emitted a message.");
        Car(talk, switch)
      })
    );

  /**
   * Control center.
   */
  def Control(talk1: Chan[unit], switch1: Switch,
              gain1: Switch, lose1: Switch, 
              talk2: Chan[unit], switch2: Switch,
              gain2: Switch, lose2: Switch): unit
  = {
    def Control1: unit= {
      Thread.sleep(1 + random.nextInt(1000));
      lose1.write(Pair(talk2, switch2));
      gain2.write(Pair(talk2, switch2));
      Control2
    }
    def Control2: unit = {
      Thread.sleep(1 + random.nextInt(1000));
      lose2.write(Pair(talk1, switch1));
      gain1.write(Pair(talk1, switch1));
      Control1
    }
    Control1
  }

  /**
  * Active transmitter.
  */
  def ActiveTransmitter(id: String, talk: Chan[unit], switch: Switch,
            gain: Switch, lose: Switch): unit
  =
    choice (
      talk * (x => {
        System.out.println(id + " received a message.")
        ActiveTransmitter(id, talk, switch, gain, lose)
      }),
      lose * ({ case Pair(t, s) => {
        switch.write(Pair(t, s))
        IdleTransmitter(id, gain, lose)
      }})
    );

  /**
   * Idle transmitter.
   */
  def IdleTransmitter(id: String, gain: Switch, lose: Switch): unit = {
    val Pair(t, s) = gain.read;
    ActiveTransmitter(id, t, s, gain, lose)
  }

  def main(args: Array[String]): unit = {
    val talk1 = new Chan[unit]
    val switch1 = new Switch
    val gain1 = new Switch
    val lose1 = new Switch
    val talk2 = new Chan[unit]
    val switch2 = new Switch
    val gain2 = new Switch
    val lose2 = new Switch
    spawn <
    Car(talk1, switch1) |
    ActiveTransmitter("Transmitter 1", talk1, switch1, gain1, lose1) |
    IdleTransmitter("Transmitter 2", gain2, lose2) |
    Control(talk1, switch1, gain1, lose1, talk2, switch2, gain2, lose2) >
  }
}

/**
* Handover example with type casts.
*/
object handoverCast {

  import concurrent.pilib._;

  val random = new java.util.Random();

  /**
  * Car.
  */
  def Car(talk: Chan[Any], switch: Chan[Any]): unit =
    choice (
      switch * (o => {
        val Pair(t,s) = o.asInstanceOf[Pair[Chan[Any],Chan[Any]]]; 
        Car(t, s)
      }),
      talk(()) * ( {
        Thread.sleep(1 + random.nextInt(1000));
        System.out.println("Car emitted a message.");
        Car(talk, switch)
      })
    );

  /**
  * Control center.
  */
  def Control(talk1: Chan[Any], switch1: Chan[Any],
              gain1: Chan[Any], lose1: Chan[Any], 
              talk2: Chan[Any], switch2: Chan[Any],
              gain2: Chan[Any], lose2: Chan[Any]): unit
  = {
    def Control1: unit = {
      Thread.sleep(1 + random.nextInt(1000));
      lose1.write(Pair(talk2, switch2));
      gain2.write(Pair(talk2, switch2));
      Control2
    }
    def Control2: unit = {
      Thread.sleep(1 + random.nextInt(1000));
      lose2.write(Pair(talk1, switch1));
      gain1.write(Pair(talk1, switch1));
      Control1
    }
    Control1
  }

  /**
   * Active transmitter.
   */
  def ActiveTransmitter(id: String, talk: Chan[Any], switch: Chan[Any],
                        gain: Chan[Any], lose: Chan[Any]): unit
  =
    choice (
      talk * (x => {
        System.out.println(id + " received a message.")
        ActiveTransmitter(id, talk, switch, gain, lose)
      }),
      lose * (o => {
        val Pair(t, s) = o.asInstanceOf[Pair[Chan[Any],Chan[Any]]]
        switch.write(Pair(t, s))
        IdleTransmitter(id, gain, lose)
      })
    )

  /**
  * Idle transmitter.
  */
  def IdleTransmitter(id: String, gain: Chan[Any], lose: Chan[Any]): unit = {
    val Pair(t, s) = gain.read.asInstanceOf[Pair[Chan[Any],Chan[Any]]]
    ActiveTransmitter(id, t, s, gain, lose)
  }

  def main(args: Array[String]): unit = {
    val talk1 = new Chan[Any]
    val switch1 = new Chan[Any]
    val gain1 = new Chan[Any]
    val lose1 = new Chan[Any]
    val talk2 = new Chan[Any]
    val switch2 = new Chan[Any]
    val gain2 = new Chan[Any]
    val lose2 = new Chan[Any]
    spawn <
    Car(talk1, switch1) |
    ActiveTransmitter("Transmitter 1", talk1, switch1, gain1, lose1) |
    IdleTransmitter("Transmitter 2", gain2, lose2) |
    Control(talk1, switch1, gain1, lose1, talk2, switch2, gain2, lose2) >
  }

}
