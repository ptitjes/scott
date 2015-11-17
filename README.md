# Scott – A part-of-speech tagger

It supports different trainers and decoders.

To train it against the FTB with a perceptron with Lefff-enhanced features, you can do:

```java io.github.ptitjes.scott.nl.lang.fr.trainFTBFineLE <path-to-ftb> <path-to-lefff>```

You'll obtain a model with 97.50% accuracy (91.49% on unknown words).
