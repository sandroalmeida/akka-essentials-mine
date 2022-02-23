package playground

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object AkkaExercisePractice extends App {

	val system = ActorSystem("akkaPractice")

	object Counter {
		case object Increment
		case object Decrement
		case object Print
	}
	class Counter extends Actor {
		import Counter._

		override def receive: Receive = countReceive(0)

		def countReceive(currentCount: Int): Receive = {
			case Increment =>
				println(s"incrementing $currentCount")
				context.become(countReceive(currentCount + 1))
			case Decrement =>
				println(s"decrementing $currentCount")
				context.become(countReceive(currentCount - 1))
			case Print => println(s"[counter] current value is $currentCount")
		}
	}

	val counter = system.actorOf(Props[Counter], "MyCounter")
	import Counter._
	(1 to 5).foreach(_ => counter ! Increment)
	(1 to 3).foreach(_ => counter ! Decrement)
	counter ! Print

	case class Vote(candidate: String)
	case object VoteStatusRequest
	case class VoteStatusReply(candidate: Option[String])

	class Citizen extends Actor {
		override def receive: Receive = {
			case Vote(c) => context.become(voted(c))
			case VoteStatusRequest => sender() ! VoteStatusReply(None)
		}
		def voted(candidate: String): Receive = {
			case VoteStatusRequest => sender() ! VoteStatusReply(Some(candidate))
		}
	}

	case class AggregateVotes(citizens: Set[ActorRef])
	class VoteAggregator extends Actor {
		override def receive: Receive = awaitingCommand
		def awaitingCommand: Receive = {
			case AggregateVotes(citizens) =>
				citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
				context.become(awaitingStatuses(citizens, Map()))
		}
		def awaitingStatuses(stillWaiting: Set[ActorRef], currentStatus: Map[String, Int]): Receive = {
			case VoteStatusReply(None) => sender() ! VoteStatusRequest
			case VoteStatusReply(Some(candidate)) =>
				val newStillWaiting = stillWaiting - sender()
				val currentVotesOfCandidate = currentStatus.getOrElse(candidate, 0)
				val newStatus = currentStatus + (candidate -> (currentVotesOfCandidate + 1))
				if(newStillWaiting.isEmpty)
					println(s"[aggregator] poll status $newStatus")
				else
					context.become(awaitingStatuses(newStillWaiting, newStatus))
		}
	}

	val alice = system.actorOf(Props[Citizen])
	val bob = system.actorOf(Props[Citizen])
	val charlie = system.actorOf(Props[Citizen])
	val daniel = system.actorOf(Props[Citizen])

	alice ! Vote("Martin")
	bob ! Vote("Jonas")
	charlie ! Vote("Roland")
	daniel ! Vote("Martin")

	val voteAggregator = system.actorOf(Props[VoteAggregator])
	voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))


}
