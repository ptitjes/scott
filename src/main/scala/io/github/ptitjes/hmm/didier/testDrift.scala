package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm._

object testDrift extends App {

	import io.github.ptitjes.hmm.Corpora._
	import io.github.ptitjes.hmm.Utils._
	import io.github.ptitjes.hmm.analysis.Results._

	val driftingSequence = Corpus(Seq(
		AnnotatedSequence(
			Array(
				5422,
				20159,
				10566,
				3217,
				25743,
				15988,
				3698,
				2,
				15706,
				26170,
				9715,
				11627,
				-1,
				8882,
				2,
				18703,
				24915,
				26220,
				13499,
				10163,
				8),
			Array(
				2,
				8,
				3,
				14,
				0,
				4,
				7,
				10,
				4,
				7,
				14,
				14,
				14,
				1,
				10,
				2,
				8,
				4,
				0,
				7,
				10))))

	val trainCorpus = timed("Open train corpus") {
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
	}

	val devCorpus = timed("Open dev corpus") {
		Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))
	}

	//  val test = devCorpus
	val test = driftingSequence

	val conf = Configuration().set(Trainer.ORDER, 3)

	val trainer = RelFreqTrainer.instantiate(conf)
	val decoder = FullDecoder.instantiate(conf)

	println(trainDecodeAndCheck(trainer, decoder, trainCorpus, test /*, true*/))
}
