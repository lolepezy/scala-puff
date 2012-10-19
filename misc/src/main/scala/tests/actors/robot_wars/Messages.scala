package tests.actors.robot_wars

import akka.actor.ActorRef

case class Shoot
case class Start
case class Damage(quality: Int)

case class NewPostion(position : Position, robot : ActorRef)