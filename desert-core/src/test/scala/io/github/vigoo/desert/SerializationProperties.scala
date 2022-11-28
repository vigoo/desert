package io.github.vigoo.desert

import io.github.vigoo.desert.custom._
import zio.URIO
import zio.test.Assertion._
import zio.test._

trait SerializationProperties {
  def canBeSerialized[R <: TestConfig, A: BinaryCodec](
      rv: Gen[R, A],
      test: Option[A => Assertion[A]] = None
  ): URIO[R, TestResult] =
    check(rv) { value =>
      val resultValue =
        for {
          serialized  <- serializeToArray(value)
          resultValue <- deserializeFromArray[A](serialized)
        } yield resultValue

      test match {
        case Some(chk) =>
          assert(resultValue)(isRight(chk(value)))
        case None      =>
          assert(resultValue)(isRight(equalTo(value)))
      }
    }

  def canBeSerializedAndReadBack[A: BinarySerializer, B: BinaryDeserializer](value: A, check: Assertion[B])(implicit
      typeRegistry: TypeRegistry
  ): TestResult = {
    val resultValue =
      for {
        serialized  <- serializeToArray(value, typeRegistry)
        resultValue <- deserializeFromArray[B](serialized, typeRegistry)
      } yield resultValue

    assert(resultValue)(isRight(check))
  }

  def failsWhenReadBack[A: BinarySerializer, B: BinaryDeserializer](value: A, check: Assertion[DesertFailure])(implicit
      typeRegistry: TypeRegistry
  ): TestResult = {
    val resultValue =
      for {
        serialized  <- serializeToArray(value, typeRegistry)
        resultValue <- deserializeFromArray[B](serialized, typeRegistry)
      } yield resultValue

    assert(resultValue)(isLeft(check))
  }

  def canBeSerializedAndReadBack[A: BinarySerializer, B: BinaryDeserializer](value: A, expectedB: B)(implicit
      typeRegistry: TypeRegistry
  ): TestResult =
    canBeSerializedAndReadBack(value, equalTo(expectedB))

  def cannotBeSerializedAndReadBack[A: BinarySerializer, B: BinaryDeserializer](
      value: A
  )(implicit typeRegistry: TypeRegistry): TestResult = {
    val resultValue =
      for {
        serialized  <- serializeToArray(value, typeRegistry)
        resultValue <- deserializeFromArray[B](serialized, typeRegistry)
      } yield resultValue

    assert(resultValue)(isLeft)
  }

  def canBeSerializedAndReadBackWithTypeTag(value: Any, expected: Any)(implicit
      typeRegistry: TypeRegistry
  ): TestResult = {
    val resultValue =
      for {
        serialized  <- serializeUnknownToArray(value, typeRegistry)
        resultValue <- deserializeUnknownFromArray(serialized, typeRegistry)
      } yield resultValue

    assert(resultValue)(isRight(equalTo(expected)))
  }
}
