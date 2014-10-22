package io.github.ptitjes.hmm.analyses

import io.github.ptitjes.hmm.{Corpora, Decoder, Trainer, didier}

object test extends App {

  val trainCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.train.encode"))
  val devCorpus = Corpora.fromURL(getClass.getResource("/data/ftb.dev.encode"))

  // Parameters
  val algorithms: Map[String, (Trainer, Decoder)] = Map(
    "Didier" ->(didier.RelFreqSimpleTrainer, didier.ParDecoder)
  )
  val depths = 1 to 4

  algorithms.foreach { case (algoName, algo) =>
    depths.foreach { depth =>
      runTest(algoName, algo, depth)
    }
  }

  def runTest(algoName: String, algo: (Trainer, Decoder), depth: Int): Unit = {
    val hmm = algo._1.train(15, depth, trainCorpus)
    val results = algo._2.decodeAndCheck(hmm, devCorpus)

    println(f"Algorithm: $algoName; Depth: $depth; " + results)
  }
}
