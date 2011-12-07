package akka.event;


import akka.actor.ActorRef;

/**
 * Java API for Akka EventHandler
 */

public class JavaEventHandler {


    public static void notify(Object message){
        EventHandler$.MODULE$.notify(message);
    }

    public static void debug(ActorRef instance, Object message){
        EventHandler$.MODULE$.debug(instance, message);
    }

    public static void info(ActorRef instance, Object message){
        EventHandler$.MODULE$.info(instance,message);
    }

    public static void warning(ActorRef instance, Object message){
        EventHandler$.MODULE$.warning(instance,message);
    }

    public static void error(ActorRef instance, Object message){
        EventHandler$.MODULE$.debug(instance,message);
    }

}


