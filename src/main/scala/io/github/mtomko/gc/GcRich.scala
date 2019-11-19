package io.github.mtomko.gc

import java.nio.file.Path

import cats.effect.{Blocker, ContextShift, ExitCode, IO, Sync}
import cats.effect.Console.io._
import cats.implicits._
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import com.monovore.decline.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import fs2.{io, text, Pipe, Stream}

object GcRich
  extends CommandIOApp(
    name = "gc-rich",
    header = "Calculates GC-richness for a FASTQ file",
    version = "0.0.1"
  ) {

  type UInt = Int Refined NonNegative

  private[this] val DefaultBufferSize: UInt = 65536

  private[this] val fastqOpt = Opts.option[Path]("fastq", short = "f", help = "The FASTQ file")

  private[this] val bufferSizeOpt =
    Opts.option[UInt]("buffer", short = "s", help = "The number of bytes to buffer").withDefault(DefaultBufferSize)

  override def main: Opts[IO[ExitCode]] =
    (fastqOpt, bufferSizeOpt).mapN { (f, b) =>
      for {
        (num, den) <- statStream[IO](f, b).compile.foldMonoid
        _ <- putStrLn(ratio(num, den, "% GC"))
      } yield ExitCode.Success
    }

  private[this] def ratio(numerator: Long, denominator: Long, label: String): String =
    if (denominator < 1) "Empty data set"
    else {
      val ratio = numerator.toDouble / denominator.toDouble * 100
      s"$ratio$label"
    }

  // average # of records is is 130,000,000 - they're frequently as big as 250,000,000
  private[this] def statStream[F[_]: Sync: ContextShift](fastqPath: Path, bufferSize: UInt): Stream[F, (Long, Long)] =
    for {
      implicit0(blocker: Blocker) <- Stream.resource(Blocker[F])
      rec <- fastqSeqs[F](fastqPath, bufferSize)
    } yield (rec.count(c => c === 'G' || c === 'C').toLong, rec.length.toLong)

  private[this] def fastqSeqs[F[_]: Sync: ContextShift](path: Path, bufferSize: UInt)(
      implicit blocker: Blocker): Stream[F, String] =
    lines[F](path, bufferSize).through(fastqSeq)

  private[this] def lines[F[_]: Sync: ContextShift](p: Path, bufferSize: UInt)(
      implicit blocker: Blocker): Stream[F, String] =
    io.file
      .readAll[F](p, blocker, bufferSize)
      .through(text.utf8Decode)
      .through(text.lines)

  private[this] def fastqSeq[F[_]]: Pipe[F, String, String] =
    _.chunkN(4, allowFewer = false).map(seg => seg(1))

}
