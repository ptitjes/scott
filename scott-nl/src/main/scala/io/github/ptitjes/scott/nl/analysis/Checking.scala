package io.github.ptitjes.scott.nl.analysis

import java.io.{File, FileWriter, PrintWriter}

import io.github.ptitjes.scott.api._
import io.github.ptitjes.scott.nl.corpora.Corpora._
import io.github.ptitjes.scott.nl.corpora.Lexica
import io.github.ptitjes.scott.utils.Utils._

object Checking {

	def check(hmm: HiddenMarkovModel[_, _],
	          refCorpus: DataSet[NLToken with NLPosTag],
	          hypCorpus: DataSet[NLToken with NLPosTag],
	          checkFile: File): Results = {

		using(new FileWriter(checkFile)) {
			fileOutput => using(new PrintWriter(fileOutput)) {
				out => check(hmm, refCorpus, hypCorpus, Some(out))
			}
		}
	}

	def check(hmm: HiddenMarkovModel[_, _],
	          refCorpus: DataSet[NLToken with NLPosTag],
	          hypCorpus: DataSet[NLToken with NLPosTag],
	          writer: Option[PrintWriter] = None): Results = {

		val tagSet: TagSet = refCorpus.tagSet

		val globalCounts = new ErrorCount
		val perCategoryCounts = Array.fill(tagSet.tags.size)(new ErrorCount)
		val confusionMatrix = Array.fill(hmm.breadth)(Array.ofDim[Int](hmm.breadth))

		val lexiconSize = Lexica.WORDS.words.length
		val perWordErrorCounts = Array.fill(lexiconSize)(new ErrorCount)
		val perWordConfusionMatrix = Array.fill(lexiconSize)(
			Array.fill(hmm.breadth)(Array.ofDim[Int](hmm.breadth))
		)

		val progress = new ProgressBar(f"Checking", refCorpus.size)
		progress.set(0)

		refCorpus.sequences.zip(hypCorpus.sequences).foreach {
			case (refSeq, hypSeq) =>

				if (refSeq.length != hypSeq.length) {
					throw new IllegalStateException("Observable length mismatch!")
				}

				val iterator = refSeq.zippedHistoryIterator(hypSeq)
				while (iterator.hasNext) {
					val (refHistory, hypHistory) = iterator.next()

					val (oRef, sRef) = (refHistory.current.word, refHistory.current.tag)
					val (oHyp, sHyp) = (hypHistory.current.word, hypHistory.current.tag)

					if (oRef != oHyp) {
						throw new IllegalStateException("Observable mismatch!")
					}
					val error = sRef != sHyp

					globalCounts.total += 1
					perCategoryCounts(sRef).total += 1
					perWordErrorCounts(oRef.code).total += 1
					if (error) {
						globalCounts.errors += 1
						perCategoryCounts(sRef).errors += 1
						confusionMatrix(sRef)(sHyp) += 1
						perWordErrorCounts(oRef.code).errors += 1
						perWordConfusionMatrix(oRef.code)(sRef)(sHyp) += 1
					}

					if (hmm.isUnknown(oRef.code)) {
						globalCounts.unknownTotal += 1
						perCategoryCounts(sRef).unknownTotal += 1
						perWordErrorCounts(oRef.code).unknownTotal += 1
						if (error) {
							globalCounts.unknownErrors += 1
							perCategoryCounts(sRef).unknownErrors += 1
							perWordErrorCounts(oRef.code).unknownErrors += 1
						}
					}

					writer.foreach { out =>
						out.print(if (error) ">" else " ")
						out.print(f"${oRef.code}%6d")
						out.print(if (hmm.isUnknown(oRef.code)) " U" else "  ")
						out.print(f"\t$sRef%2d ${tagSet(sRef)}%-5s")
						out.print(f"\t$sHyp%2d ${tagSet(sHyp)}%-5s\t")
						out.println(oRef.string)
					}
				}

				writer.foreach(out => out.println())

				progress.increment()
		}

		val allPerWord = (0 until lexiconSize)
			.map(i => (Lexica.WORDS(i), perWordErrorCounts(i), perWordConfusionMatrix(i)))
			.toList

		val top50KnownMostFrequent = allPerWord
			.filter { case (w, errorCounts, confusion) => errorCounts.unknownTotal == 0}
			.sortBy { case (w, errorCounts, confusion) => -errorCounts.total}
			.take(50)

		val top50UnknownMostFrequent = allPerWord
			.filter { case (w, errorCounts, confusion) => errorCounts.unknownTotal != 0}
			.sortBy { case (w, errorCounts, confusion) => -errorCounts.total}
			.take(50)

		val top50Known = allPerWord
			.filter { case (w, errorCounts, confusion) => errorCounts.unknownTotal == 0}
			.sortBy { case (w, errorCounts, confusion) => -errorCounts.errors}
			.take(50)

		val top50Unknown = allPerWord
			.filter { case (w, errorCounts, confusion) => errorCounts.unknownTotal != 0}
			.sortBy { case (w, errorCounts, confusion) => -errorCounts.errors}
			.take(50)

		val results = Results(globalCounts, perCategoryCounts, confusionMatrix,
			top50KnownMostFrequent, top50UnknownMostFrequent, top50Known, top50Unknown)

		writer.foreach(out => results.printTo(out, tagSet))

		results
	}
}
