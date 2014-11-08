package io.github.ptitjes.hmm.analysis

import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.didier._

object testDrift extends App {

  import io.github.ptitjes.hmm.Corpora._
  import io.github.ptitjes.hmm.Utils._

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
    Corpora.fromURL(getClass.getResource("/data/ftb.train.encode"))
  }

  val devCorpus = timed("Open dev corpus") {
    Corpora.fromURL(getClass.getResource("/data/ftb.dev.encode"))
  }

  //  val test = devCorpus
  val test = driftingSequence

  val conf = Configuration().set(Trainer.ORDER, 3)

  val trainer = RelFreqSimpleTrainer.instantiate(conf)
  val decoder = ParDecoder.instantiate(conf)

  val hmm = timed("Train HMM") {
    trainer.train(15, trainCorpus)
  }

  import io.github.ptitjes.hmm.analysis.Results._

  timed("Test HMM") {
    val results = decodeAndCheck(decoder, hmm, test)
    println(results)
  }
}
