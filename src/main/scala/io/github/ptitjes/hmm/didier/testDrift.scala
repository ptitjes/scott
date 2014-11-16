package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._

object testDrift extends App {

	import io.github.ptitjes.hmm.Corpora._
	import io.github.ptitjes.hmm.Utils._
	import io.github.ptitjes.hmm.analysis.Results._

	val driftingSequence = Corpus(Array(
		AnnotatedSequence(
			Array(
				(5422, 2),
				(20159, 8),
				(10566, 3),
				(3217, 14),
				(25743, 0),
				(15988, 4),
				(3698, 7),
				(2, 10),
				(15706, 4),
				(26170, 7),
				(9715, 14),
				(11627, 14),
				(21994, 14),
				(8882, 1),
				(2, 10),
				(18703, 2),
				(24915, 8),
				(26220, 4),
				(13499, 0),
				(10163, 7),
				(8, 10)
			).map { case (c, t) => (Word(c, Lexica.WORDS(c)), t)}
		)
	))

	val trainCorpus = timed("Open train corpus") {
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"), Lexica.WORDS)
	}

	val devCorpus = timed("Open dev corpus") {
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"), Lexica.WORDS)
	}

	//val test = devCorpus
	val test = driftingSequence

	val conf = Configuration().set(Trainer.ORDER, 3)

	val trainer = RelFreqTrainer.instantiate(conf)
	val decoder = FullDecoder.instantiate(conf)

	println(trainDecodeAndCheck(trainer, decoder, trainCorpus, test, true))
}
