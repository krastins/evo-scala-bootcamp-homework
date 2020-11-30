package actors

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  private def forwardToChild(m: Operation): Unit =
    if (m.elem < elem && subtrees.isDefinedAt(Left))
      subtrees(Left) ! m
    else if (m.elem > elem && subtrees.isDefinedAt(Right))
      subtrees(Right) ! m

  override def receive: Receive = {
    case i @ Insert(_, _, _) => doInsert(i)
    case c @ Contains(_, _, _) => doContains(c)
    case r @ Remove(_, _, _) => doRemove(r)
  }

  private def doInsert(m: Insert): Unit = {
    val maybePosition: Option[Position] = if (m.elem > elem) Some(Right) else if (m.elem < elem) Some(Left) else None

    maybePosition match {
      case Some(position) =>
        subtrees.get(position) match {
          case Some(actor) => actor ! m
          case None =>
            subtrees = subtrees + (position -> context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false)))
            m.requester ! OperationFinished(m.id)
        }
      case None =>
        if(removed) removed = false
        m.requester ! OperationFinished(m.id)
    }
  }

  private def doContains(m: Contains): Unit = {
    if (elem == m.elem)
      m.requester ! ContainsResult(id = m.id, result = !removed)
    else if(subtrees.isEmpty)
      m.requester ! ContainsResult(id = m.id, result = false)
    else forwardToChild(m)
  }

  private def doRemove(m: Remove): Unit = {
    if (elem == m.elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    }
    else forwardToChild(m)
  }
}
