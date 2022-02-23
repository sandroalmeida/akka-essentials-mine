package playground

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import playground.Playground.Person.LiveTheLife

object Playground extends App {

	val system = ActorSystem("AkkaPlayground")

	object Counter {
		case object Increment
		case object Decrement
		case object Print
	}
	class Counter extends Actor {
		import Counter._

		var count = 0
		override def receive: Receive = {
			case Increment => count += 1
			case Decrement => count -= 1
			case Print => println(s"[counter] My current counter is $count")
		}
	}

	val counter = system.actorOf(Props[Counter], "MyCounter")
	import Counter._
	(1 to 5).foreach(_ => counter ! Increment)
	(1 to 3).foreach(_ => counter ! Decrement)
	counter ! Print

	object BankAccount {
		case class Deposit(amount: Int)
		case class Withdraw(amount: Int)
		case object Statement

		case class TransactionSuccess(message: String)
		case class TransactionFailure(reason: String)
	}
	class BankAccount extends Actor {
		import BankAccount._
		var funds = 0

		override def receive: Receive = {
			case Deposit(amount) =>
				if(amount <= 0)
					sender() ! TransactionFailure("invalid deposit amount")
				else {
					funds += amount
					sender() ! TransactionSuccess(s"successfully deposit $amount")
				}
			case Withdraw(amount) =>
				if(amount <= 0)
					sender() ! TransactionFailure("invalid deposit amount")
				else if(amount > funds)
					sender() ! TransactionFailure("insufficient funds to complete transaction")
				else {
					funds -= amount
					sender() ! TransactionSuccess(s"successfully withdrew $amount")
				}
			case Statement => sender() ! s"Your current balance is $funds"
		}
	}

	object Person {
		case class LiveTheLife(account: ActorRef)
	}
	class Person extends Actor {
		import Person._
		import BankAccount._

		override def receive: Receive = {
			case LiveTheLife(account) =>
				account ! Deposit(10000)
				account ! Withdraw(90000)
				account ! Withdraw(500)
				account ! Statement
			case message => println(message.toString)
		}
	}

	val account = system.actorOf(Props[BankAccount], "bankAccount")
	val person = system.actorOf(Props[Person], "millionaire")
	person ! LiveTheLife(account)

}
