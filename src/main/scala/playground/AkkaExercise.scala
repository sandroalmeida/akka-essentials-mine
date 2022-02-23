package playground

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import playground.AkkaExercise.Mom.{CHOCOLATE, Food, VEGETABLES}

object AkkaExercise extends App{

	object FussyKid {
		case object KidAccept
		case object KidReject
		val HAPPY = "happy"
		val SAD = "sad"
	}
	class FussyKid extends Actor {
		import FussyKid._
		import Mom._

		var state: String = HAPPY
		override def receive: Receive = {
			case Food(VEGETABLES) => state = SAD
			case Food(CHOCOLATE) => state = HAPPY
			case Ask(_) =>
				if(state == HAPPY) sender() ! KidAccept
				else sender() ! KidReject
		}
	}
	class StatelessFussyKid extends Actor {
		import Mom._
		import FussyKid._

		override def receive: Receive = happyReceive
		def happyReceive: Receive = {
			case Food(VEGETABLES) => context.become(sadReceive)
			case Food(CHOCOLATE) => //do nothing
			case Ask(_) => sender() ! KidAccept
		}
		def sadReceive: Receive = {
			case Food(VEGETABLES) => //do nothing
			case Food(CHOCOLATE) => context.become(happyReceive)
			case Ask(_) => sender() ! KidReject
		}
	}
	object Mom {
		case class MomStart(kidRef: ActorRef)
		case class Food(food: String)
		case class Ask(message: String)
		val VEGETABLES = "veggies"
		val CHOCOLATE = "chocolate"
	}
	class Mom extends Actor {
		import Mom._
		import FussyKid._
		override def receive: Receive = {
			case MomStart(kidRef) =>
				kidRef ! Food(VEGETABLES)
				kidRef ! Ask("do you wanna play?")
			case KidAccept => println("Yay! my kid is happy!")
			case KidReject => println("My kid is sad, but at least his is healthy")
		}
	}

	val system = ActorSystem("changingActorBehaviour")
	val fussyKid = system.actorOf(Props[FussyKid], "kid")
	val statelessFussyKid = system.actorOf(Props[StatelessFussyKid], "statelessKid")
	val mom = system.actorOf(Props[Mom], "mom")

	import Mom._
	mom ! MomStart(fussyKid)


}
