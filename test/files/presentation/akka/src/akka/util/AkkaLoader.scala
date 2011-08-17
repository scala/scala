/**
 * Copyright (C) 2009-2011 Scalable Solutions AB <http://scalablesolutions.se>
 */

package akka.util

import akka.config.Config
import akka.actor.Actor

/*
 * This class is responsible for booting up a stack of bundles and then shutting them down
 */
class AkkaLoader {
  private val hasBooted = new Switch(false)

  @volatile
  private var _bundles: Option[Bootable] = None

  def bundles = _bundles;

  /*
   * Boot initializes the specified bundles
   */
  def boot(withBanner: Boolean, b: Bootable): Unit = hasBooted switchOn {
    if (withBanner) printBanner()
    println("Starting Akka...")
    b.onLoad
    Thread.currentThread.setContextClassLoader(getClass.getClassLoader)
    _bundles = Some(b)
    println("Akka started successfully")
  }

  /*
   * Shutdown, well, shuts down the bundles used in boot
   */
  def shutdown() {
    hasBooted switchOff {
      println("Shutting down Akka...")
      _bundles.foreach(_.onUnload)
      _bundles = None
      Actor.shutdownHook.run
      println("Akka succesfully shut down")
    }
  }

  private def printBanner() {
    println("""
==============================================================================

                                                   ZZ:
                                                  ZZZZ
                                                 ZZZZZZ
                                                ZZZ' ZZZ
                                       ~7      7ZZ'   ZZZ
                                      :ZZZ:   IZZ'     ZZZ
                                     ,OZZZZ.~ZZ?        ZZZ
                                    ZZZZ' 'ZZZ$          ZZZ
                           .       $ZZZ   ~ZZ$            ZZZ
                         .=Z?.   .ZZZO   ~ZZ7              OZZ
                        .ZZZZ7..:ZZZ~   7ZZZ                ZZZ~
                      .$ZZZ$Z+.ZZZZ    ZZZ:                  ZZZ$
                   .,ZZZZ?'  =ZZO=   .OZZ                     'ZZZ
                 .$ZZZZ+   .ZZZZ    IZZZ                        ZZZ$
               .ZZZZZ'   .ZZZZ'   .ZZZ$                          ?ZZZ
            .ZZZZZZ'   .OZZZ?    ?ZZZ                             'ZZZ$
        .?ZZZZZZ'    .ZZZZ?    .ZZZ?                                'ZZZO
    .+ZZZZZZ?'    .7ZZZZ'    .ZZZZ                                    :ZZZZ
 .ZZZZZZ$'     .?ZZZZZ'   .~ZZZZ                                        'ZZZZ.


                      NNNNN              $NNNN+
                      NNNNN              $NNNN+
                      NNNNN              $NNNN+
                      NNNNN              $NNNN+
                      NNNNN              $NNNN+
    =NNNNNNNNND$      NNNNN     DDDDDD:  $NNNN+     DDDDDN     NDDNNNNNNNN,
   NNNNNNNNNNNNND     NNNNN    DNNNNN    $NNNN+   8NNNNN=    :NNNNNNNNNNNNNN
  NNNNN$    DNNNNN    NNNNN  $NNNNN~     $NNNN+  NNNNNN      NNNNN,   :NNNNN+
   ?DN~      NNNNN    NNNNN MNNNNN       $NNNN+:NNNNN7        $ND      =NNNNN
            DNNNNN    NNNNNDNNNN$        $NNNNDNNNNN                  :DNNNNN
     ZNDNNNNNNNNND    NNNNNNNNNND,       $NNNNNNNNNNN           DNDNNNNNNNNNN
   NNNNNNNDDINNNNN    NNNNNNNNNNND       $NNNNNNNNNNND       ONNNNNNND8+NNNNN
 :NNNND      NNNNN    NNNNNN  DNNNN,     $NNNNNO 7NNNND     NNNNNO     :NNNNN
 DNNNN       NNNNN    NNNNN    DNNNN     $NNNN+   8NNNNN    NNNNN      $NNNNN
 DNNNNO     NNNNNN    NNNNN     NNNNN    $NNNN+    NNNNN$   NNNND,    ,NNNNND
  NNNNNNDDNNNNNNNN    NNNNN     =NNNNN   $NNNN+     DNNNN?  DNNNNNNDNNNNNNNND
   NNNNNNNNN  NNNN$   NNNNN      8NNNND  $NNNN+      NNNNN=  ,DNNNNNNND NNNNN$

==============================================================================
                      Running version %s
==============================================================================
""".format(Config.VERSION))
  }
}
