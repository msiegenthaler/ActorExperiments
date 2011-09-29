package io.console

import annotation._
import actor._
import actor.iteratee._
import execution._
import io.iteratee._
import Iteratee._
import java.util.concurrent.atomic._

/**
 * Reads lines from the console (System.in) and forwards them to all subscribed Iteratees.
 * The actor runs in a separate thread and is started as long as there are subscriptions.
 */
object ConsoleReader extends ActorImplementor {
  case class Subscribe(subscriber: ActionIteratee[String])

  val actor: Actor[Subscribe] = {
    new ActorImpl[Subscribe] {
      override def processMessage(msg: Subscribe) = msg match {
        case Subscribe(it) ⇒ addNewSubscriber(it)
      }
    }
  }
  private type Subscriber = ActionIteratee[String]
  private case class State(running: Boolean, subs: List[Subscriber])

  private val stateRef = new AtomicReference(State(false, Nil))
  @tailrec private def addNewSubscriber(sub: Subscriber) {
    val s = stateRef.get
    if (s.running) {
      if (stateRef.compareAndSet(s, State(true, sub :: s.subs))) () //added to state
      else addNewSubscriber(sub)
    } else {
      if (stateRef.compareAndSet(s, State(true, Nil))) Thread(namedExecution("ConsoleReader")(run(sub :: s.subs)))
      else addNewSubscriber(sub)
    }
  }

  @tailrec private def run(subs: List[Subscriber]): Unit = {
    val in = Console.readLine
    // 'prefetch' new subscribers, so already receive the new line. Since we're the only thread sending 
    //  to or removing subscribers this is safe
    val subs2cont = handle(Data(in))(subs ::: stateRef.get.subs)
    val nextSubs = setSubscribers(subs2cont)
    if (!nextSubs.isEmpty) run(nextSubs)
    else () // no more subscribers, terminate the reader thread
  }
  @tailrec def setSubscribers(existingSubs: List[Subscriber]): List[Subscriber] = {
    val s = stateRef.get
    val nsubs = existingSubs ::: s.subs
    if (stateRef.compareAndSet(s, State(!nsubs.isEmpty, Nil))) nsubs
    else setSubscribers(existingSubs)
  }

  @tailrec def handle(data: Input[String])(left: List[Subscriber], handled: List[Subscriber] = Nil): List[Subscriber] = left match {
    case it :: l ⇒
      val next = process(it, data)
      if (next.isDone) handle(data)(l, handled)
      else handle(data)(l, next :: handled)
    case Nil ⇒ handled //let's not reverse, since we don't guarantee order anyway
  }
  @tailrec def process(it: Subscriber, data: Input[String]): Subscriber = it match {
    case Cont(c) ⇒
      val next = c(data)
      next.outOption.foreach(handleAction)
      processCallAgains(next)
    case CallAgain(c) ⇒
      val next = c(Empty)
      next.outOption.foreach(handleAction)
      process(next, data)
    case Done(d) ⇒ done
  }
  @tailrec def processCallAgains(it: Subscriber): Subscriber = it match {
    case CallAgain(c) ⇒
      val next = c(Empty)
      next.outOption.foreach(handleAction)
      processCallAgains(next)
    case Cont(c) ⇒ it
    case Done(d) ⇒ done
  }
}