-- See https://github.com/probmods/webppl/blob/dev/examples/lda.wppl

import Probability

vocabulary = ["bear", "wolf", "python", "prolog"];

ntopics = 2

docs = map words [
        "bear wolf bear wolf bear wolf python wolf bear wolf",
        "python prolog python prolog python prolog python prolog python prolog",
        "bear wolf bear wolf bear wolf bear wolf bear wolf",
        "python prolog python prolog python prolog python prolog python prolog",
        "bear wolf bear python bear wolf bear wolf bear wolf"]

doc_lengths = map length docs

word_frequencies_dist = symmetric_dirichlet_on vocabulary 1.0

word_dist_for_doc word_frequencies_for_topic nwords = do
  topic_frequencies <- symmetric_dirichlet ntopics 1.0
  topics  <- iid nwords $ categorical topic_frequencies
  let word_dist = independent [ categorical_on $ word_frequencies_for_topic!!topic | topic <- topics ]
  return (word_dist, topic_frequencies)

model = do
  word_frequencies_for_topic <- iid ntopics word_frequencies_dist
      
  (word_dists, topic_frequencies) <- unzip `liftM` independent [word_dist_for_doc word_frequencies_for_topic nwords | nwords <- doc_lengths]

  let loggers = ["word_frequencies_for_topic" %=% word_frequencies_for_topic,
                 "topic_frequencies_for_doc" %=% topic_frequencies]

  return (word_dists, loggers)


main = do
  (word_dists, loggers) <- random $ model
  observe (independent word_dists) docs
  return loggers
