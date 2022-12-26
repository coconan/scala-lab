package actorbintree

import actorbintree.BinaryTreeNode.*
import actorbintree.BinaryTreeSet
import actorbintree.BinaryTreeSet.*
import akka.actor.{ActorSystem, Props}

object Playground extends App {
  val system = ActorSystem("playground")

  val actor1 = system.actorOf(Props[BinaryTreeSet]())
  val actorSet = system.actorOf(Props[BinaryTreeSet]())

  actorSet ! Insert(actor1, 1, 1)
  actorSet ! Insert(actor1, 2, 2)
  actorSet ! Remove(actor1, 3, 2)
  actorSet ! Print
  actorSet ! GC
  actorSet ! Print
  actorSet ! Contains(actor1, 4, 1)

}