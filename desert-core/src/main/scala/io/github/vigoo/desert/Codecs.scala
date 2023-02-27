package io.github.vigoo.desert

import java.nio.charset.StandardCharsets
import java.util.UUID
import io.github.vigoo.desert.custom._
import io.github.vigoo.desert.internal.SerializerState.{StringAlreadyStored, StringId, StringIsNew}
import io.github.vigoo.desert.internal.{AdtCodec, OptionBinaryCodec}

import java.time._
import scala.annotation.tailrec
import scala.collection.{Factory, mutable}
import scala.collection.immutable.{ArraySeq, SortedMap, SortedSet}
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** Module containing implicit binary codecs for a lot of base types
  */
trait Codecs extends internal.TupleCodecs {

  implicit val byteCodec: BinaryCodec[Byte] = new BinaryCodec[Byte] {
    override def deserialize()(implicit ctx: DeserializationContext): Byte            = readByte()
    override def serialize(value: Byte)(implicit context: SerializationContext): Unit = writeByte(value)
  }

  implicit val shortCodec: BinaryCodec[Short] = new BinaryCodec[Short] {
    override def deserialize()(implicit ctx: DeserializationContext): Short            = readShort()
    override def serialize(value: Short)(implicit context: SerializationContext): Unit = writeShort(value)
  }

  implicit val intCodec: BinaryCodec[Int] = new BinaryCodec[Int] {
    override def deserialize()(implicit ctx: DeserializationContext): Int            = readInt()
    override def serialize(value: Int)(implicit context: SerializationContext): Unit = writeInt(value)
  }

  implicit val longCodec: BinaryCodec[Long] = new BinaryCodec[Long] {
    override def deserialize()(implicit ctx: DeserializationContext): Long            = readLong()
    override def serialize(value: Long)(implicit context: SerializationContext): Unit = writeLong(value)
  }

  implicit val floatCodec: BinaryCodec[Float] = new BinaryCodec[Float] {
    override def deserialize()(implicit ctx: DeserializationContext): Float            = readFloat()
    override def serialize(value: Float)(implicit context: SerializationContext): Unit = writeFloat(value)
  }

  implicit val doubleCodec: BinaryCodec[Double] = new BinaryCodec[Double] {
    override def deserialize()(implicit ctx: DeserializationContext): Double            = readDouble()
    override def serialize(value: Double)(implicit context: SerializationContext): Unit = writeDouble(value)
  }

  private def createVarIntCodec(optimizeForPositive: Boolean): BinaryCodec[Int] = new BinaryCodec[Int] {
    override def deserialize()(implicit ctx: DeserializationContext): Int            =
      readVarInt(optimizeForPositive)
    override def serialize(value: Int)(implicit context: SerializationContext): Unit =
      writeVarInt(value, optimizeForPositive)
  }

  val varIntOptimizedForPositiveCodec: BinaryCodec[Int] = createVarIntCodec(optimizeForPositive = true)
  val varIntCodec: BinaryCodec[Int]                     = createVarIntCodec(optimizeForPositive = false)

  implicit val booleanCodec: BinaryCodec[Boolean] = new BinaryCodec[Boolean] {
    override def deserialize()(implicit ctx: DeserializationContext): Boolean =
      readByte() != 0

    override def serialize(value: Boolean)(implicit context: SerializationContext): Unit = writeByte(
      if (value) 1 else 0
    )
  }

  implicit val unitCodec: BinaryCodec[Unit] = new BinaryCodec[Unit] {
    override def deserialize()(implicit ctx: DeserializationContext): Unit            = ()
    override def serialize(value: Unit)(implicit context: SerializationContext): Unit = ()
  }

  implicit val charCodec: BinaryCodec[Char] = BinaryCodec.from(
    shortCodec.contramap(_.toShort),
    shortCodec.map(_.toChar)
  )

  implicit val stringCodec: BinaryCodec[String] = new BinaryCodec[String] {
    private val charset = StandardCharsets.UTF_8

    override def deserialize()(implicit ctx: DeserializationContext): String = {
      val count = readVarInt(optimizeForPositive = false)
      val bytes = readBytes(count)
      try
        new String(bytes, charset)
      catch {
        case NonFatal(reason) =>
          throw DesertException(DesertFailure.DeserializationFailure("Failed to decode string", Some(reason)))
      }
    }

    override def serialize(value: String)(implicit context: SerializationContext): Unit = {
      val raw = value.getBytes(charset)
      writeVarInt(raw.size, optimizeForPositive = false)
      writeBytes(raw)
    }
  }

  implicit val deduplicatedStringCodec: BinaryCodec[DeduplicatedString] = new BinaryCodec[DeduplicatedString] {
    private val charset = StandardCharsets.UTF_8

    override def deserialize()(implicit ctx: DeserializationContext): DeduplicatedString = {
      val countOrId = readVarInt(optimizeForPositive = false)
      if (countOrId < 0) {
        val id = StringId(-countOrId)
        getString(id) match {
          case Some(string) => DeduplicatedString(string)
          case None         => throw DesertException(DesertFailure.InvalidStringId(id))
        }
      } else {
        val bytes  = readBytes(countOrId)
        val string =
          try
            new String(bytes, charset)
          catch {
            case NonFatal(reason) =>
              throw DesertException(DesertFailure.DeserializationFailure("Failed to decode string", Some(reason)))
          }
        storeReadString(string)
        DeduplicatedString(string)
      }
    }

    override def serialize(value: DeduplicatedString)(implicit context: SerializationContext): Unit =
      storeString(value.string) match {
        case StringAlreadyStored(id) =>
          writeVarInt(-id.value, optimizeForPositive = false)
        case StringIsNew(id)         =>
          stringCodec.serialize(value.string)
      }
  }

  implicit val uuidCodec: BinaryCodec[UUID] = new BinaryCodec[UUID] {
    override def deserialize()(implicit ctx: DeserializationContext): UUID = {
      val mostSigBits  = readLong()
      val leastSigBits = readLong()
      new UUID(mostSigBits, leastSigBits)
    }

    override def serialize(value: UUID)(implicit context: SerializationContext): Unit = {
      writeLong(value.getMostSignificantBits)
      writeLong(value.getLeastSignificantBits)
    }
  }

  implicit val bigDecimalCodec: BinaryCodec[BigDecimal] =
    BinaryCodec.from(stringCodec.contramap(_.toString()), stringCodec.map(BigDecimal(_)))

  implicit val javaBigDecimalCodec: BinaryCodec[java.math.BigDecimal] =
    BinaryCodec.from(stringCodec.contramap(_.toString()), stringCodec.map(new java.math.BigDecimal(_)))

  implicit val javaBigIntegerCodec: BinaryCodec[java.math.BigInteger] = new BinaryCodec[java.math.BigInteger] {
    override def deserialize()(implicit ctx: DeserializationContext): java.math.BigInteger = {
      val length = readVarInt(optimizeForPositive = true)
      val bytes  = readBytes(length)
      new java.math.BigInteger(bytes)
    }

    override def serialize(value: java.math.BigInteger)(implicit context: SerializationContext): Unit = {
      val bytes = value.toByteArray
      writeVarInt(bytes.length, optimizeForPositive = true)
      writeBytes(bytes)
    }
  }

  implicit val bigIntCodec: BinaryCodec[BigInt] =
    BinaryCodec.from(
      javaBigIntegerCodec.contramap(_.bigInteger),
      javaBigIntegerCodec.map(BigInt(_))
    )

  implicit val dayOfWeekCodec: BinaryCodec[DayOfWeek] =
    BinaryCodec.from(
      byteCodec.contramap(_.getValue.toByte),
      byteCodec.map(n => DayOfWeek.of(n.toInt))
    )

  implicit val monthCodec: BinaryCodec[Month] =
    BinaryCodec.from(
      byteCodec.contramap(_.getValue.toByte),
      byteCodec.map(n => Month.of(n.toInt))
    )

  implicit val yearCodec: BinaryCodec[Year] =
    BinaryCodec.from(
      varIntOptimizedForPositiveCodec.contramap(_.getValue),
      varIntOptimizedForPositiveCodec.map(n => Year.of(n))
    )

  implicit val monthDayCodec: BinaryCodec[MonthDay] =
    new BinaryCodec[MonthDay] {
      override def deserialize()(implicit ctx: DeserializationContext): MonthDay = {
        val month      = read[Month]()
        val dayOfMonth = readByte()
        MonthDay.of(month, dayOfMonth.toInt)
      }

      override def serialize(value: MonthDay)(implicit context: SerializationContext): Unit = {
        write(value.getMonth)
        writeByte(value.getDayOfMonth.toByte)
      }
    }

  implicit val yearMonthCodec: BinaryCodec[YearMonth] = new BinaryCodec[YearMonth] {
    override def deserialize()(implicit ctx: DeserializationContext): YearMonth = {
      val year  = readVarInt(optimizeForPositive = true)
      val month = read[Month]()
      YearMonth.of(year, month)
    }

    override def serialize(value: YearMonth)(implicit context: SerializationContext): Unit = {
      writeVarInt(value.getYear, optimizeForPositive = true)
      write(value.getMonth)
    }
  }

  implicit val periodCodec: BinaryCodec[Period] =
    new BinaryCodec[Period] {
      override def deserialize()(implicit ctx: DeserializationContext): Period = {
        val years  = readVarInt(optimizeForPositive = true)
        val months = readVarInt(optimizeForPositive = true)
        val days   = readVarInt(optimizeForPositive = true)
        Period.of(years, months, days)
      }

      override def serialize(value: Period)(implicit context: SerializationContext): Unit = {
        writeVarInt(value.getYears, optimizeForPositive = true)
        writeVarInt(value.getMonths, optimizeForPositive = true)
        writeVarInt(value.getDays, optimizeForPositive = true)
      }
    }

  implicit val zoneIdCodec: BinaryCodec[ZoneId] =
    new BinaryCodec[ZoneId] {
      override def deserialize()(implicit ctx: DeserializationContext): ZoneId = {
        val typ = readByte()
        typ match {
          case 0 => ZoneOffset.ofTotalSeconds(readVarInt(optimizeForPositive = false))
          case 1 => ZoneId.of(read[String]())
          case _ => throw DesertException(DesertFailure.InvalidConstructorId(typ.toInt, "ZoneId"))
        }
      }

      override def serialize(value: ZoneId)(implicit context: SerializationContext): Unit =
        value match {
          case offset: ZoneOffset =>
            writeByte(0)
            writeVarInt(offset.getTotalSeconds, optimizeForPositive = false)
          case region             =>
            writeByte(1)
            write(region.getId)
        }
    }

  implicit val zoneOffsetCodec: BinaryCodec[ZoneOffset] =
    BinaryCodec.from(
      varIntCodec.contramap(_.getTotalSeconds),
      varIntCodec.map(ZoneOffset.ofTotalSeconds)
    )

  implicit val durationCodec: BinaryCodec[Duration] =
    new BinaryCodec[Duration] {
      override def deserialize()(implicit ctx: DeserializationContext): Duration = {
        val seconds = readLong()
        val nanos   = readInt()
        Duration.ofSeconds(seconds, nanos.toLong)
      }

      override def serialize(value: Duration)(implicit context: SerializationContext): Unit = {
        writeLong(value.getSeconds)
        writeInt(value.getNano)
      }
    }

  implicit val instantCodec: BinaryCodec[Instant] = new BinaryCodec[Instant] {
    override def deserialize()(implicit ctx: DeserializationContext): Instant = {
      val seconds = readLong()
      val nanos   = readInt()
      Instant.ofEpochSecond(seconds, nanos.toLong)
    }

    override def serialize(value: Instant)(implicit context: SerializationContext): Unit = {
      writeLong(value.getEpochSecond)
      writeInt(value.getNano)
    }
  }

  implicit val localDateCodec: BinaryCodec[LocalDate] =
    new BinaryCodec[LocalDate] {
      override def deserialize()(implicit ctx: DeserializationContext): LocalDate = {
        val year  = readVarInt(optimizeForPositive = true)
        val month = read[Month]()
        val day   = readByte()
        LocalDate.of(year, month, day.toInt)
      }

      override def serialize(value: LocalDate)(implicit context: SerializationContext): Unit = {
        writeVarInt(value.getYear, optimizeForPositive = true)
        write(value.getMonth)
        writeByte(value.getDayOfMonth.toByte)
      }
    }

  implicit val localTimeCodec: BinaryCodec[LocalTime] =
    new BinaryCodec[LocalTime] {
      override def deserialize()(implicit ctx: DeserializationContext): LocalTime = {
        val hour   = readByte()
        val minute = readByte()
        val second = readByte()
        val nano   = readVarInt(optimizeForPositive = true)
        LocalTime.of(hour, minute, second, nano)
      }

      override def serialize(value: LocalTime)(implicit context: SerializationContext): Unit = {
        writeByte(value.getHour.toByte)
        writeByte(value.getMinute.toByte)
        writeByte(value.getSecond.toByte)
        writeVarInt(value.getNano, optimizeForPositive = true)
      }
    }

  implicit val localDateTimeCodec: BinaryCodec[LocalDateTime] =
    new BinaryCodec[LocalDateTime] {
      override def deserialize()(implicit ctx: DeserializationContext): LocalDateTime = {
        val date = read[LocalDate]()
        val time = read[LocalTime]()
        LocalDateTime.of(date, time)
      }

      override def serialize(value: LocalDateTime)(implicit context: SerializationContext): Unit = {
        write(value.toLocalDate)
        write(value.toLocalTime)
      }
    }

  implicit val offsetTimeCodec: BinaryCodec[OffsetTime] =
    new BinaryCodec[OffsetTime] {
      override def deserialize()(implicit ctx: DeserializationContext): OffsetTime = {
        val time   = read[LocalTime]()
        val offset = read[ZoneOffset]()
        OffsetTime.of(time, offset)
      }

      override def serialize(value: OffsetTime)(implicit context: SerializationContext): Unit = {
        write(value.toLocalTime)
        write(value.getOffset)
      }
    }

  implicit val offsetDateTimeCodec: BinaryCodec[OffsetDateTime] =
    new BinaryCodec[OffsetDateTime] {
      override def deserialize()(implicit ctx: DeserializationContext): OffsetDateTime = {
        val dateTime = read[LocalDateTime]()
        val offset   = read[ZoneOffset]()
        OffsetDateTime.of(dateTime, offset)
      }

      override def serialize(value: OffsetDateTime)(implicit context: SerializationContext): Unit = {
        write(value.toLocalDateTime)
        write(value.getOffset)
      }
    }

  implicit val zonedDateTimeCodec: BinaryCodec[ZonedDateTime] =
    new BinaryCodec[ZonedDateTime] {
      override def deserialize()(implicit ctx: DeserializationContext): ZonedDateTime = {
        val dateTime = read[LocalDateTime]()
        val offset   = read[ZoneOffset]()
        val zone     = read[ZoneId]()
        ZonedDateTime.ofStrict(dateTime, offset, zone)
      }

      override def serialize(value: ZonedDateTime)(implicit context: SerializationContext): Unit = {
        write(value.toLocalDateTime)
        write(value.getOffset)
        write(value.getZone)
      }
    }

  implicit def optionCodec[T: BinaryCodec]: BinaryCodec[Option[T]] = OptionBinaryCodec[T]()

  // Throwable

  implicit val stackTraceElementCodec: BinaryCodec[StackTraceElement] =
    new BinaryCodec[StackTraceElement] {
      override def deserialize()(implicit ctx: DeserializationContext): StackTraceElement = {
        val _          = readByte()
        val className  = read[Option[String]]()
        val methodName = read[Option[String]]()
        val fileName   = read[Option[String]]()
        val lineNumber = readVarInt(optimizeForPositive = true)
        new StackTraceElement(className.orNull, methodName.orNull, fileName.orNull, lineNumber)
      }

      override def serialize(value: StackTraceElement)(implicit context: SerializationContext): Unit = {
        writeByte(0)
        write(Option(value.getClassName))
        write(Option(value.getMethodName))
        write(Option(value.getFileName))
        writeVarInt(value.getLineNumber, optimizeForPositive = true)
      }
    }

  implicit def persistedThrowableCodec: BinaryCodec[PersistedThrowable] =
    new AdtCodec[PersistedThrowable, PersistedThrowable](
      evolutionSteps = Vector(Evolution.InitialVersion),
      typeName = "io.github.vigoo.desert.PersistedThrowable",
      constructors = Vector("PersistedThrowable"),
      transientFields = Map.empty,
      getSerializationCommands = (pt: PersistedThrowable) =>
        List(
          AdtCodec.SerializationCommand.WriteField("className", pt.className, () => stringCodec),
          AdtCodec.SerializationCommand.WriteField("message", pt.message, () => stringCodec),
          AdtCodec.SerializationCommand.WriteField("stackTrace", pt.stackTrace, () => arrayCodec[StackTraceElement]),
          AdtCodec.SerializationCommand.WriteField("cause", pt.cause, () => optionCodec(persistedThrowableCodec))
        ),
      deserializationCommands = List(
        AdtCodec.DeserializationCommand.Read(
          "className",
          () => stringCodec,
          (className: String, pt: PersistedThrowable) => pt.copy(className = className)
        ),
        AdtCodec.DeserializationCommand
          .Read("message", () => stringCodec, (message: String, pt: PersistedThrowable) => pt.copy(message = message)),
        AdtCodec.DeserializationCommand.Read(
          "stackTrace",
          () => arrayCodec[StackTraceElement],
          (stackTrace: Array[StackTraceElement], pt: PersistedThrowable) => pt.copy(stackTrace = stackTrace)
        ),
        AdtCodec.DeserializationCommand.ReadOptional(
          "cause",
          () => persistedThrowableCodec,
          () => optionCodec(persistedThrowableCodec),
          (cause: Option[PersistedThrowable], pt: PersistedThrowable) => pt.copy(cause = cause)
        )
      ),
      initialBuilderState = () => PersistedThrowable("", "", Array.empty, None),
      materialize = Right(_)
    )

  implicit val throwableCodec: BinaryCodec[Throwable] = BinaryCodec.from(
    persistedThrowableCodec.contramap {
      case persistedThrowable: PersistedThrowable => persistedThrowable
      case throwable: Throwable                   => PersistedThrowable(throwable)
    },
    persistedThrowableCodec.map(persisted => persisted)
  )

  // Collections

  def iterableCodec[A: BinaryCodec, T <: Iterable[A]](implicit factory: Factory[A, T]): BinaryCodec[T] =
    new BinaryCodec[T] {
      override def deserialize()(implicit ctx: DeserializationContext): T = {
        val knownSize = readVarInt(optimizeForPositive = false)
        if (knownSize == -1)
          deserializeWithUnknownSize()
        else
          deserializeWithKnownSize(knownSize)
      }

      private def deserializeWithUnknownSize()(implicit ctx: DeserializationContext): T = {
        val builder = factory.newBuilder
        readAll(builder)
        builder.result()
      }

      @tailrec
      private def readAll(builder: mutable.Builder[A, T])(implicit ctx: DeserializationContext): Unit =
        read[Option[A]]() match {
          case Some(value) =>
            builder += value
            readAll(builder)
          case None        =>
        }

      private def deserializeWithKnownSize(size: Int)(implicit ctx: DeserializationContext): T = {
        val builder = factory.newBuilder
        builder.sizeHint(size)
        for (_ <- 0 until size)
          builder += read[A]()
        builder.result()
      }

      override def serialize(value: T)(implicit context: SerializationContext): Unit =
        if (value.knownSize == -1)
          serializeWithUnknownSize(value)
        else
          serializeWithKnownSize(value, value.knownSize)

      private def serializeWithUnknownSize(value: T)(implicit context: SerializationContext): Unit = {
        writeVarInt(-1, optimizeForPositive = false)
        for (elem <- value) {
          write(true)
          write(elem)
        }
        write(false)
      }

      private def serializeWithKnownSize(value: T, size: Int)(implicit context: SerializationContext): Unit = {
        writeVarInt(size, optimizeForPositive = false)
        for (elem <- value)
          write(elem)
      }
    }

  implicit def wrappedArrayCodec[A: BinaryCodec: ClassTag]: BinaryCodec[ArraySeq[A]] = iterableCodec[A, ArraySeq[A]]

  implicit def arrayCodec[A: BinaryCodec: ClassTag]: BinaryCodec[Array[A]] = BinaryCodec.from(
    wrappedArrayCodec[A].contramap(arr => ArraySeq.unsafeWrapArray(arr)),
    wrappedArrayCodec[A].map(_.toArray[A])
  )

  implicit def listCodec[A: BinaryCodec]: BinaryCodec[List[A]] = iterableCodec[A, List[A]]

  implicit def vectorCodec[A: BinaryCodec]: BinaryCodec[Vector[A]] = iterableCodec[A, Vector[A]]

  implicit def setCodec[A: BinaryCodec]: BinaryCodec[Set[A]] = iterableCodec[A, Set[A]]

  implicit def sortedSetCodec[A: BinaryCodec: Ordering]: BinaryCodec[SortedSet[A]] = iterableCodec[A, SortedSet[A]]

  implicit def mapCodec[K: BinaryCodec, V: BinaryCodec]: BinaryCodec[Map[K, V]] = iterableCodec[(K, V), Map[K, V]]

  implicit def sortedMapCodec[K: BinaryCodec: Ordering, V: BinaryCodec]: BinaryCodec[SortedMap[K, V]] =
    iterableCodec[(K, V), SortedMap[K, V]]

  implicit def eitherCodec[L: BinaryCodec, R: BinaryCodec]: BinaryCodec[Either[L, R]] = new BinaryCodec[Either[L, R]] {

    override def deserialize()(implicit ctx: DeserializationContext): Either[L, R] = {
      val isRight = read[Boolean]()
      if (isRight) Right(read[R]()) else Left(read[L]())
    }

    override def serialize(value: Either[L, R])(implicit context: SerializationContext): Unit =
      value match {
        case Left(value)  =>
          write(false)
          write(value)
        case Right(value) =>
          write(true)
          write(value)
      }
  }

  implicit def tryCodec[A: BinaryCodec]: BinaryCodec[Try[A]] = new BinaryCodec[Try[A]] {

    override def deserialize()(implicit ctx: DeserializationContext): Try[A] = {
      val isSuccess = read[Boolean]()
      if (isSuccess) Success(read[A]()) else Failure(read[Throwable]())
    }

    override def serialize(value: Try[A])(implicit context: SerializationContext): Unit =
      value match {
        case Failure(reason) => write(false); write(reason)
        case Success(value)  => write(true); write(value)
      }
  }
}
