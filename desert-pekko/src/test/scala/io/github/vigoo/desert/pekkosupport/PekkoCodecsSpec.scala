package io.github.vigoo.desert.pekkosupport

import org.apache.pekko
import pekko.actor.typed.scaladsl.Behaviors
import pekko.actor.typed.scaladsl.adapter._
import pekko.actor.typed.{ActorRef, ActorSystem, Behavior}
import pekko.actor.{
  Actor => UntypedActor,
  ActorRef => UntypedActorRef,
  ActorSystem => UntypedActorSystem,
  ExtendedActorSystem => UntypedExtendedActorSystem,
  Props => UntypedProps
}
import io.github.vigoo.desert._
import zio.test.Assertion._
import zio.test._
import zio._

object PekkoCodecsSpec extends ZIOSpecDefault {

  override def spec =
    suite("Akka serialization codecs")(
      test("correctly serializes untyped actor references")(
        untypedActorSystem.flatMap { system =>
          implicit val extendedSystem: UntypedExtendedActorSystem = system.asInstanceOf[UntypedExtendedActorSystem]
          for {
            actor1     <- ZIO.attempt(system.actorOf(UntypedProps(classOf[TestUntypedActor])))
            serialized <- ZIO.fromEither(serializeToArray(actor1)).mapError(new DesertException(_))
            actor2     <- ZIO.fromEither(deserializeFromArray[UntypedActorRef](serialized)).mapError(new DesertException(_))
          } yield assert(actor1)(equalTo(actor2))
        }
      ),
      test("correctly serializes typed actor references")(
        typedActorSystem.flatMap { implicit system =>
          for {
            actor1     <- ZIO.attempt(system.toClassic.spawnAnonymous(testActorBehavior))
            serialized <- ZIO.fromEither(serializeToArray(actor1)).mapError(new DesertException(_))
            actor2     <-
              ZIO.fromEither(deserializeFromArray[ActorRef[String]](serialized)).mapError(new DesertException(_))
          } yield assert(actor1)(equalTo(actor2))
        }
      )
    )

  private def untypedActorSystem: ZIO[Scope, Throwable, UntypedActorSystem] =
    ZIO.acquireRelease(ZIO.attempt(UntypedActorSystem("test")))(system =>
      ZIO.fromFuture(_ => system.terminate()).unit.catchAll(logFatalError)
    )

  private def typedActorSystem: ZIO[Scope, Throwable, ActorSystem[_]] =
    untypedActorSystem.map(_.toTyped)

  private def logFatalError(reason: Throwable): ZIO[Any, Nothing, Unit] =
    Console.printLine(s"Fatal actor termination error: ${reason.getMessage}").orDie

  class TestUntypedActor extends UntypedActor {
    override def receive: Receive = ???
  }

  val testActorBehavior: Behavior[String] =
    Behaviors.receiveMessage(_ => Behaviors.ignore)
}
