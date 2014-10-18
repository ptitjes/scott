package io.github.ptitjes.hmm

import java.io.File

object test extends App {

  val PATH = "/home/didier/Documents/Work/Master/Docs/Inférence Statistique/Alexis Nasr/Code HMM/"

  val trainCorpus = timed("Open train corpus file") {
    Corpora.fromFile(new File(PATH + "ftb.train.encode"))
  }

  val devCorpus = timed("Open dev corpus file") {
    Corpora.fromFile(new File(PATH + "ftb.dev.encode"))
  }

  val testCorpus = timed("Open test corpus file") {
    Corpora.fromFile(new File(PATH + "ftb.test.encode"))
  }

  val algo: Algorithms = didier.STImplementations

  val hmm = timed("Train HMM") {
    algo.trainWithRelativeFrequence(15, 3, trainCorpus, devCorpus)
  }

  timed("Test HMM") {
    val results = Algorithms.mostProbableStateSequences(algo, hmm, testCorpus)
    println(results)
  }

  def timed[T](step: String)(execution: => T): T = {
    val start = System.currentTimeMillis()
    val result = execution
    val time = System.currentTimeMillis() - start
    println(step + ": " + time + "ms")
    result
  }
}
