package scala.concurrent;

object NameServer {
  val names=new scala.collection.mutable.HashMap[Symbol,Process];

  def register(name:Symbol,proc:Process) = {
      if (names.contains(name)) error("Name:" + name + " already registred");
      names += name -> proc;
  }

  def unregister(name:Symbol) = {
      if (names.contains(name))
        names -= name;
      else
        error("Name:" + name + " not registred");
  }

  def whereis(name:Symbol):Option[Process] =
      names.get(name);

  def send(name:Symbol, msg:Actor#Message) = {
      names(name).send(msg)
  }

}
