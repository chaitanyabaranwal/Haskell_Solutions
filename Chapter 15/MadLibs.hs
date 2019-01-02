module MadLibs where

    type Verb = String
    type Noun = String
    type Adjective = String
    type Adverb = String
    type Exclamation = String

    madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
    madlibbin' e adv noun adj = 
        e <> "! he said " <>
        adv <> " as he jumped in his car " <>
        noun <> " and drove off with his " <> adj <> " wife."

    madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
    madlibbinBetter' e adv noun adj = mconcat [e, "! he said ", adv, 
                                        " as he jumped in his car ", noun, " and drove off with his ", adj, " wife."]