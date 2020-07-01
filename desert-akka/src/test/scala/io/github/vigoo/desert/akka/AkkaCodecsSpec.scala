package io.github.vigoo.desert.akka

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.adapter._
import akka.actor.{Actor => UntypedActor, ActorRef => UntypedActorRef, ActorSystem => UntypedActorSystem, ExtendedActorSystem => UntypedExtendedActorSystem, Props => UntypedProps}
import io.github.vigoo.desert.DesertException
import io.github.vigoo.desert.syntax._
import io.github.vigoo.desert.akka.codecs._
import org.junit.runner.RunWith
import zio.console.Console
import zio.{Task, ZIO, ZManaged, console}
import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment

import scala.annotation.nowarn

@RunWith(classOf[zio.test.junit.ZTestJUnitRunner])
class AkkaCodecsSpec extends DefaultRunnableSpec {
  import io.github.vigoo.desert.akka.AkkaCodecsSpec._

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Akka serialization codecs")(
      testM("correctly serializes untyped actor references")(
        untypedActorSystem.use { system =>
          implicit val extendedSystem: UntypedExtendedActorSystem = system.asInstanceOf[UntypedExtendedActorSystem]
          for {
            actor1 <- Task(system.actorOf(UntypedProps(classOf[TestUntypedActor])))
            serialized <- ZIO.fromEither(serializeToArray(actor1)).mapError(new DesertException(_))
            actor2 <- ZIO.fromEither(deserializeFromArray[UntypedActorRef](serialized)).mapError(new DesertException(_))
          } yield assert(actor1)(equalTo(actor2))
        }
      ),
      testM("correctly serializes typed actor references")(
        typedActorSystem.use { implicit system =>
          for {
            actor1 <- Task(system.toClassic.spawnAnonymous(testActorBehavior))
            serialized <- ZIO.fromEither(serializeToArray(actor1)).mapError(new DesertException(_))
            actor2 <- ZIO.fromEither(deserializeFromArray[ActorRef[String]](serialized)).mapError(new DesertException(_))
          } yield assert(actor1)(equalTo(actor2))
        }
      )
    )

  private def untypedActorSystem: ZManaged[TestEnvironment, Throwable, UntypedActorSystem] =
    ZManaged.make(Task(UntypedActorSystem("test")))(
      system => ZIO.fromFuture(_ => system.terminate()).unit.catchAll(logFatalError))

  private def typedActorSystem: ZManaged[TestEnvironment, Throwable, ActorSystem[_]] =
    untypedActorSystem.map(_.toTyped)

  private def logFatalError(reason: Throwable): ZIO[Console, Nothing, Unit] =
    console.putStrLn(s"Fatal actor termination error: ${reason.getMessage}")

}

@nowarn object AkkaCodecsSpec extends AkkaCodecsSpec {
  class TestUntypedActor extends UntypedActor {
    override def receive: Receive = ???
  }

  val testActorBehavior: Behavior[String] =
    Behaviors.receiveMessage( _ => Behaviors.ignore )
}
