package io.github.ptitjes.hmm.didier

import io.github.ptitjes.hmm.Utils._
import io.github.ptitjes.hmm._
import io.github.ptitjes.hmm.analysis.Results._
import io.github.ptitjes.hmm.analysis._

object testDiscriminant extends App {

  val trainCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode"))

  val conf = Configuration()
    .set(Trainer.ORDER, 1)
    .set(DiscriminantTrainer.ITERATION_COUNT, 1)

  val trainer = DiscriminantTrainer.instantiate(conf)
  val decoder = FullDecoder.instantiate(conf)

  val hmm = timed("Train HMM") {
    trainer.train(trainCorpus)
  }

  timed("Test HMM") {
    println(decodeAndCheck(decoder, hmm, devCorpus))
  }

  import io.github.ptitjes.hmm.analysis.ConfigurationSet._

  implicit val runner: AnalysisRunner = new AnalysisRunner("report/results-discriminant.json",
    Corpora.annotatedFrom(getClass.getResource("/data/ftb.train.encode")),
    Corpora.annotatedFrom(getClass.getResource("/data/ftb.dev.encode")))

  val iterationAnalysis =
    (Analysis.TRAINER as didier.DiscriminantTrainer) *
      (Analysis.DECODER as didier.FullDecoder) *
      (Trainer.ORDER as 1) *
      (DiscriminantTrainer.ITERATION_COUNT from (1 to 40))

  runner.resultsFor(iterationAnalysis)
}
