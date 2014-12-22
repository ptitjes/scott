package io.github.ptitjes.scott.analysis

import java.io.{File, FileWriter, PrintWriter}

import io.github.ptitjes.scott.corpora.{TagSet, Corpora}
import Corpora._
import io.github.ptitjes.scott.Utils._
import io.github.ptitjes.scott._
import io.github.ptitjes.scott.decoders.BeamDecoder

case class Results(globalCounts: ErrorCount,
                   perCategoryCounts: Array[ErrorCount],
                   confusionMatrix: Array[Array[Int]],
                   top50KnownMostFrequent: List[(Word, ErrorCount, Array[Array[Int]])],
                   top50UnknownMostFrequent: List[(Word, ErrorCount, Array[Array[Int]])],
                   top50Known: List[(Word, ErrorCount, Array[Array[Int]])],
                   top50Unknown: List[(Word, ErrorCount, Array[Array[Int]])]) {

	def errorRate = globalCounts.errorRate

	def accuracy = globalCounts.accuracy

	def unknownErrorRate = globalCounts.unknownErrorRate

	def unknownAccuracy = globalCounts.unknownAccuracy

	def unknownErrorRatio = globalCounts.unknownErrorRatio

	def categoryErrorRatio(i: Int) = perCategoryCounts(i).errors.toDouble / globalCounts.errors

	def display(): Unit = {
		println(this)
	}

	def printTo(out: PrintWriter, tagSet: TagSet): Unit = {
		out.println(this)

		for (i <- 0 until perCategoryCounts.length)
			out.println(f"\tCategory: ${
				tagSet.padded(i)
			} ${
				perCategoryCounts(i)
			}; Of Total; ${
				categoryErrorRatio(i) * 100
			}%6.2f%%")
		out.println()

		out.println("Confusion matrix")
		out.print(" " * 5)
		for (j <- 0 until confusionMatrix.length) {
			out.print(s"\t${tagSet.padded(j)}")
		}
		out.println()
		for (i <- 0 until confusionMatrix.length) {
			out.print(s"${tagSet.padded(i)}")
			for (j <- 0 until confusionMatrix(i).length) {
				out.print(f"\t${confusionMatrix(i)(j)}%5d")
			}
			out.println()
		}
		out.println()

		out.println("Top 50 of most frequent known words")
		top50KnownMostFrequent
			.foreach { case (w, errorCounts, confusion) =>
			out.println(f"${w.string}%-20s ${errorCounts.toString}")
		}
		out.println()

		out.println("Top 50 of most frequent unknown words")
		top50UnknownMostFrequent
			.foreach { case (w, errorCounts, confusion) =>
			out.println(f"${w.string}%-20s ${errorCounts.toString}")
		}
		out.println()

		out.println("Top 50 of erroneous known words")
		top50Known
			.foreach { case (w, errorCounts, confusion) =>
			out.println(f"${w.string}%-20s ${errorCounts.toString}")
		}
		out.println()

		out.println("Top 50 of erroneous unknown words")
		top50Unknown
			.foreach { case (w, errorCounts, confusion) =>
			out.println(f"${w.string}%-20s ${errorCounts.toString}")
		}
		out.println()
	}

	override def toString: String = globalCounts.toString
}

class ErrorCount {
	var errors: Int = 0
	var total: Int = 0
	var unknownErrors: Int = 0
	var unknownTotal: Int = 0

	def errorRate = errors.toDouble / total.toDouble

	def accuracy = 1 - errorRate

	def unknownErrorRate = unknownErrors.toDouble / unknownTotal.toDouble

	def unknownAccuracy = 1 - unknownErrorRate

	def unknownErrorRatio = unknownErrors.toDouble / errors.toDouble

	override def toString: String =
		f"Errors: ${
			errors
		}%5d; Total: ${
			total
		}%5d; UnknownErrors: ${
			unknownErrors
		}%5d; UnknownTotal: ${
			unknownTotal
		}%5d Accuracy: ${
			accuracy * 100
		}%6.2f%%; UnknownAccuracy: ${
			unknownAccuracy * 100
		}%6.2f%%; UnknownErrorRatio: ${
			unknownErrorRatio * 100
		}%6.2f%%"
}

object Results {

}
