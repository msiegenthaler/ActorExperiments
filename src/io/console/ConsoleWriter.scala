package io.console

import annotation._
import actor._
import execution._
import java.util.concurrent._

object ConsoleWriter extends ActorImplementor {

  val actor: Actor[String] = new ActorImpl[String] {
    override def processMessage(msg: String) = queue offer msg
  }

  private val queue = new LinkedBlockingQueue[String]

  @tailrec private def run {
    val s = queue.take
    if (s != null) Console.println(s)
    run
  }
  DaemonThread(namedExecution("ConsoleWriter")(run))
}