/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor.*

import scala.collection.immutable.Queue
import scala.util.Random

object BinaryTreeSet:

  trait Operation:
    def requester: ActorRef
    def id: Int
    def elem: Int

  trait OperationReply:
    def id: Int

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  case object Print

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply



class BinaryTreeSet extends Actor:
  import BinaryTreeSet.*
  import BinaryTreeNode.*

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional (used to stash incoming operations during garbage collection)
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case Insert(requester, id, elem) => root ! Insert(requester, id, elem)
    case Contains(requester, id, elem) => root ! Contains(requester, id, elem)
    case Remove(requester, id, elem) => root ! Remove(requester, id, elem)
    case GC =>
      // println(self)
      // println("gc start")
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
    case Print => root ! Print
    case _ =>
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case Insert(requester, id, elem) => pendingQueue = pendingQueue.enqueue(Insert(requester, id, elem))
    case Contains(requester, id, elem) => pendingQueue = pendingQueue.enqueue(Contains(requester, id, elem))
    case Remove(requester, id, elem) => pendingQueue = pendingQueue.enqueue(Remove(requester, id, elem))
    case CopyFinished =>
      // println("gc ended")
      for
        ops <- pendingQueue
      yield newRoot ! ops
      pendingQueue = Queue.empty[Operation]
      root = newRoot
      context.become(normal)
    case _ =>
  }


object BinaryTreeNode:
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  /**
   * Acknowledges that a copy has been completed. This message should be sent
   * from a node to its parent, when this node and all its children nodes have
   * finished being copied.
   */
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor:
  import BinaryTreeNode.*
  import BinaryTreeSet.*

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, elem) =>
      if this.elem > elem then
        if subtrees.contains(Left) then subtrees(Left) ! Insert(requester, id, elem)
        else
          subtrees = subtrees + (Left -> context.actorOf(BinaryTreeNode.props(elem, initiallyRemoved = false)))
          requester ! OperationFinished(id)
      else if this.elem < elem then
        if subtrees.contains(Right) then subtrees(Right) ! Insert(requester, id, elem)
        else
          subtrees = subtrees + (Right -> context.actorOf(BinaryTreeNode.props(elem, initiallyRemoved = false)))
          requester ! OperationFinished(id)
      else
        removed = false
        requester ! OperationFinished(id)
    case Contains(requester, id, elem) =>
      if this.elem == elem && !removed then requester ! ContainsResult(id, true)
      else if this.elem > elem && subtrees.contains(Left) then subtrees(Left) ! Contains(requester, id, elem)
      else if this.elem < elem && subtrees.contains(Right) then subtrees(Right) ! Contains(requester, id, elem)
      else requester ! ContainsResult(id, false)
    case Remove(requester, id, elem) =>
      if this.elem > elem then
        if subtrees.contains(Left) then subtrees(Left) ! Remove(requester, id, elem)
        else requester ! OperationFinished(id)
      else if this.elem < elem then
        if subtrees.contains(Right) then subtrees(Right) ! Remove(requester, id, elem)
        else requester ! OperationFinished(id)
      else
        removed = true
        requester ! OperationFinished(id)
    case CopyTo(treeNode) =>
      // println(this.elem + "start copy...")
      if !removed then
        treeNode ! Insert(self, elem, elem)
      if subtrees.contains(Left) && subtrees.contains(Right) then
        subtrees(Left) ! CopyTo(treeNode)
        subtrees(Right) ! CopyTo(treeNode)
        context.become(copying(Set(subtrees(Left), subtrees(Right)), removed))
      else if subtrees.contains(Left) then
        subtrees(Left) ! CopyTo(treeNode)
        context.become(copying(Set(subtrees(Left)), removed))
      else if subtrees.contains(Right) then
        subtrees(Right) ! CopyTo(treeNode)
        context.become(copying(Set(subtrees(Right)), removed))
      else
        if removed then
          self ! OperationFinished(elem)
        context.become(copying(Set(), removed))
    case Print =>
      // println(elem + " removed: " + removed)
      if subtrees.contains(Left) && subtrees.contains(Right) then
        subtrees(Left) ! Print
        subtrees(Right) ! Print
      else if subtrees.contains(Left) then
        subtrees(Left) ! Print
      else if subtrees.contains(Right) then
        subtrees(Right) ! Print
    case _ => ???
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) =>
      // println(elem + "self copy finished")
      // if elem == 0 then println(expected)
      if id == elem then
        if expected.isEmpty then
          // println("stop " + this.elem)
          context.parent ! CopyFinished
          context.stop(self)
        else context.become(copying(expected, true))
    case CopyFinished =>
      // println(elem + "children copy finished")
      // if elem == 0 then println((expected - sender()).toString() + "" + (insertConfirmed).toString)
      if (expected - sender()).isEmpty && insertConfirmed then
        // println("stop " + this.elem)
        context.parent ! CopyFinished
        context.stop(self)
      else
        context.become(copying(expected - sender(), insertConfirmed))
    case _ =>
  }


