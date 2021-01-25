{-# LANGUAGE RecursiveDo #-}
import           Probability
import           Tree
import           Tree.Newick

model = sample $ do
    tree <- sample_uniform_time_tree 1.0 5 -- uniform_time_tree 1.0 5
--    tree <- uniform_time_tree 1.0 5
    let ltree = add_labels tree ["a","b","c","d","e"]
    let pr = uniform_time_tree_pr 1.0 5 ltree

    let ps    = map (show . parentNode tree) [0 .. 5]

    rec let mu node = case parentNode tree node of
                Nothing   -> 0.0
                Just node -> xs !! node
        xs <- independent [ normal (mu node) 1.0 | node <- nodes tree ]
  -- can we _observe_ from this? -- why or why not?

    return ["tree" %=% write_newick ltree,"pr" %=% pr, "xs" %=% xs, "ps" %=% ps]

main = do
  mcmc model
