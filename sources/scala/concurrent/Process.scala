package scala.concurrent;

object Process {
    def spawn(def body:unit):Process = {
	val p = new Process(body);
	p.start();
	p;
    }
    def spawn_link(def body:unit):Process = {
	self.spawn_link(body);
    }

    def send(p:Process,msg:Actor#Message) =
	p.send(msg);
    def receive[a](f: PartialFunction[Actor#Message, a]): a =
	self.receive(f);

    def receiveWithin[a](msec: long)(f: PartialFunction[Actor#Message, a]):a =
        self.receiveWithin(msec)(f);

    def self:Process = {
	if (Thread.currentThread().isInstanceOf[Process])
	Thread.currentThread().asInstanceOf[Process]
	else error("Self called outside a process");
    }

    def exit(p:Process,reason:AnyRef) =
	p.exit(reason);
}

class Process(def body:unit) extends Actor() {
    private var exitReason:AnyRef = null;
    private var links:List[Process] = Nil;
    override def run() = {
	try {body}
	catch {
	    case _:java.lang.InterruptedException =>
    	      signal(exitReason);
	    case (exitSignal) =>
	      signal(exitSignal);
	}
    }

    private def signal(s:Actor#Message) = {
	links.foreach((p:Process) => p.send(Tuple3('EXIT,this,s)));
    }

    def !(msg:Actor#Message) =
	send(msg);

    def link(p:Process) = {
	links = p::links;
    }

    def spawn_link(def body:unit) = {
	val p = new Process(body);
	p.link(this);
	p.start();
	p
    }

    def self = this;

    def exit(reason:AnyRef):unit = {
	exitReason = reason;
	interrupt();
    }
}
