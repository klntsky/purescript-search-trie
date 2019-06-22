module Test.Main where

import Prelude

import Data.Array (sort)
import Data.Array as A
import Data.List ((:), List(Nil))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe as MB
import Data.Search.Trie (alter, delete, deleteByPrefix, entries, entriesUnordered, fromFoldable, insert, isEmpty, lookup, query, queryValues, size, subtrie, subtrieWithPrefixes, update, values)
import Data.Search.Trie.Internal (Trie(..), eq')
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (class Foldable, for_)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert, assertEqual')

main :: Effect Unit
main = do
  testIsEmpty
  testSize
  testInsert
  testSubtrie
  testGiantTrie
  testEntries
  testQuery
  testDelete
  testDeleteByPrefix
  testAlter
  testUpdate
  testValues

testIsEmpty :: Effect Unit
testIsEmpty = do
  assertEqual' "isEmpty #0"
    { expected: true
    , actual: isEmpty (mempty :: Trie Int Int)
    }

  assertEqual' "isEmpty #1"
    { expected: false
    , actual: isEmpty (branch [ tup 0 (single 1)
                              ])
    }

testSize :: Effect Unit
testSize = do
  assertEqual' "size #0"
    { expected: 0
    , actual: size (mempty :: Trie Int Int)
    }

  assertEqual' "size #1"
    { expected: 2
    , actual: size $
      branch [ tup 0 (single 0)
             , tup 1 (single 1)
             ]
    }

  assertEqual' "size #2"
    { expected: 3
    , actual: size $
      branch [ tup 0 (single 0)
             , tup 1 (branch [ tup 0 (single 0)
                             , tup 1 (single 1)
                             ]
                     )
             ]
    }

testInsert :: Effect Unit
testInsert = do

  let trie0 =
        insert (l [0]) 0 $
        insert (l [1]) 1 $
        mempty

  assertEq' "insert #0: Branch works"
    { expected: branch [ tup 0 (single 0)
                       , tup 1 (single 1)
                       ]
    , actual: trie0
    }

  let trie1 =
        insert (l [1,2,3,4,5,6]) 0 $
        insert (l [1,2,3,4,5,7]) 1 $
        mempty

  assertEq' "insert #1: Arc works"
    { expected: branch [ tup 1 $
                         arc [2,3,4,5] $
                         branch [ tup 6 (single 0)
                                , tup 7 (single 1)
                                ]
                       ]
    , actual: trie1
    }

testSubtrie :: Effect Unit
testSubtrie = do
  let trie1 =
        fromFoldable [ t [1,2,3,4,5] 0
                     , t [1,2,3,4,6] 1
                     , t [1,2,3]     2
                     ]

  assertEqual' "subtrie #1"
    { expected: Just
      [ t []    2
      , t [4,5] 0
      , t [4,6] 1
      ]
    , actual:
      L.toUnfoldable <<< entries <$>
      subtrie (l [1,2,3]) trie1
    }

  let trie2 =
        fromFoldable [ t [1,2,3,4,5] 0
                     , t [1,2,3,4,6] 1
                     , t [1,2,3]     2
                     , t [1,2,4]     3
                     , t [1,2,5]     4
                     ]

  assertEqual' "subtrie #2"
    { expected: Just
      [ t []    2
      , t [4,5] 0
      , t [4,6] 1
      ]
    , actual:
      L.toUnfoldable <<< entries <$>
      subtrie (l [1,2,3]) trie2
    }

  let trie3 =
        fromFoldable [ t [1,2,3,4,5,60] 0
                     , t [1,2,3,4,5,61] 1
                     , t [1,2,3,4,5,62] 2
                     ]

  assertEqual' "subtrie #3"
    { expected: Just
      [ t [4,5,60] 0
      , t [4,5,61] 1
      , t [4,5,62] 2
      ]
    , actual:
      L.toUnfoldable <<< entries <$>
      subtrie (l [1,2,3]) trie3
    }

  assertEqual' "subtrie #4"
    { expected: Just
      [ t [60] 0
      , t [61] 1
      , t [62] 2
      ]
    , actual:
      L.toUnfoldable <<< entries <$>
      subtrie (l [1,2,3,4,5]) trie3
    }

  let trie4 =
        fromFoldable [ t [1,2,3,5] 0
                     , t [1,2,3,4,5] 1
                     , t [1,2,0] 2
                     ]

  assertEqual' "subtrie #5"
    { expected: Just $
      fromFoldable [ t [1,2,3,5] 0
                   , t [1,2,3,4,5] 1
                   ]
    , actual: subtrieWithPrefixes (l [1,2,3]) trie4
    }

testGiantTrie :: Effect Unit
testGiantTrie = do
  let giantTrie = fromFoldable $ (\key -> Tuple key unit) <$> giantArray

  for_ giantArray \key -> do
    assertEqual' (fromCharArray key)
      { expected: Just unit
      , actual: lookup (l key) giantTrie
      }

  assertEqual' "lookup #0"
    { expected: Nothing
    , actual: lookup (l $ toCharArray "foo") giantTrie
    }

  let allKeys = L.toUnfoldable <<< fst <$> entries giantTrie
  assertEqual' "giantTrie" { expected: sort $ map fromCharArray giantArray
                           , actual: sort $ L.toUnfoldable $ map fromCharArray allKeys
                           }

testEntries :: Effect Unit
testEntries = do
  assertEqual' "entries #0"
    { expected: ((Tuple (1 : 2 : 3 : 4 : 5 : 60 : Nil) 0) :
                 (Tuple (1 : 2 : 3 : 4 : 5 : 61 : Nil) 1) :
                 (Tuple (1 : 2 : 3 : 4 : 5 : 62 : Nil) 2) :
                 (Tuple (1 : 2 : 4 : 4 : 5 : 62 : Nil) 2) :
                 (Tuple (10 : 20 : 30 : 40 : 50 : 60 : Nil) 3) : Nil)
    , actual: entries $
       fromFoldable [ t [1,2,3,4,5,60] 0
                    , t [1,2,3,4,5,61] 1
                    , t [10,20,30,40,50,60] 3
                    , t [1,2,3,4,5,62] 2
                    , t [1,2,4,4,5,62] 2
                    ]
    }

  assertEqual' "entries #0"
    { expected: ((Tuple (1 : 2 : 3 : 4 : 5 : 61 : Nil) 1) :
                 (Tuple (1 : 2 : 3 : 4 : 5 : 60 : Nil) 0) :
                 (Tuple (1 : 2 : 3 : 4 : 5 : 62 : Nil) 2) :
                 (Tuple (1 : 2 : 4 : 4 : 5 : 62 : Nil) 2) :
                 (Tuple (10 : 20 : 30 : 40 : 50 : 60 : Nil) 3) : Nil)
    , actual: entriesUnordered $
       fromFoldable [ t [1,2,3,4,5,60] 0
                    , t [1,2,3,4,5,61] 1
                    , t [10,20,30,40,50,60] 3
                    , t [1,2,3,4,5,62] 2
                    , t [1,2,4,4,5,62] 2
                    ]
    }

testQuery :: Effect Unit
testQuery = do
  let els = toCharArray <$>
            [ "aaa"
            , "aaaa"
            , "aaab"
            , "aaaaaaaaaaaa"
            , "aabaaaaa"
            , "bbbbaaaa"
            ]

  assertEqual' "query #0"
    { expected: A.sort
      [ "aaa"
      , "aaaa"
      , "aaab"
      , "aaaaaaaaaaaa"
      ]
    , actual:
      L.toUnfoldable $
      map snd $
      query (L.fromFoldable $ toCharArray "aaa") $
      fromFoldable $
      (\x -> Tuple x $ fromCharArray x) <$> els
    }

  assertEqual' "query #1" $
    let trie = fromFoldable $
               [ t [1,2,3] 1
               , t [1,4,5] 2
               , t [] 3
               ] in
    { expected: L.fromFoldable $
      [ t [1,2,3] 1
      , t [1,4,5] 2
      ]
    , actual:
      query (l [1]) trie
    }

  assertEqual' "query #2" $
    let trie = fromFoldable $
               [ t [1] 0
               , t [1,2,3] 1
               , t [1,4,5] 2
               , t [1,2,3,4,5] 4
               , t [] 3
               ] in
    { expected: L.fromFoldable $
      [ t [1] 0
      , t [1,2,3] 1
      , t [1,2,3,4,5] 4
      , t [1,4,5] 2
      ]
    , actual:
      query (l [1]) trie
    }

  assertEqual' "queryValues #0" $
    let trie = fromFoldable $
               [ t [1,2,3] 1
               , t [1,4,5] 2
               , t [] 3
               ] in
    { expected: L.fromFoldable [1,2]
    , actual:
      queryValues (l [1]) trie
    }

testDelete :: Effect Unit
testDelete = do

  let els = toCharArray <$>
            [ "aaa"
            , "aaaa"
            , "aaab"
            , "aaaaaaaaaaaa"
            , "aabaaaaa"
            , "bbbbaaaa"
            ]

  let trie0 :: Trie Char String
      trie0 =
        fromFoldable $
          (\x -> Tuple x $ fromCharArray x) <$> els

  let trie1 :: Trie Char String
      trie1 = delete (l $ toCharArray "aaa") trie0

  assertEqual' "delete #0"
    { expected: A.sort
      [ "aaaa"
      , "aaab"
      , "aaaaaaaaaaaa"
      , "aabaaaaa"
      , "bbbbaaaa"
      ]
    , actual: snd <$> (L.toUnfoldable (entries trie1))
    }

  -- delete nothing
  let trie2 :: Trie Char String
      trie2 = delete (l $ toCharArray "a") trie1

  assertEqual' "delete #1"
    { expected: A.sort
      [ "aaaa"
      , "aaab"
      , "aaaaaaaaaaaa"
      , "aabaaaaa"
      , "bbbbaaaa"
      ]
    , actual: snd <$> (L.toUnfoldable $ entries trie2)
    }

  let trie3 :: Trie Char String
      trie3 = delete (l $ toCharArray "bbbbaaaa") trie2

  assertEqual' "delete #2"
    { expected: A.sort
      [ "aaaa"
      , "aaab"
      , "aaaaaaaaaaaa"
      , "aabaaaaa"
      ]
    , actual: snd <$> (L.toUnfoldable $ entries trie3)
    }

  let trie4 :: Trie Char String
      trie4 = delete (l $ toCharArray "aaab") trie3

  assertEqual' "delete #3"
    { expected: A.sort
      [ "aaaa"
      , "aaaaaaaaaaaa"
      , "aabaaaaa"
      ]
    , actual: snd <$> (L.toUnfoldable $ entries trie4)
    }

  let trie5 :: Trie Char String
      trie5 = delete (l $ toCharArray "aaaa") trie4

  assertEqual' "delete #4"
    { expected: A.sort
      [ "aaaaaaaaaaaa"
      , "aabaaaaa"
      ]
    , actual: snd <$> (L.toUnfoldable $ entries trie5)
    }

  let trie6 :: Trie Char String
      trie6 = delete (l $ toCharArray "aaaaaaaaaaaa") trie5

  assertEqual' "delete #5"
    { expected: A.sort
      [ "aabaaaaa"
      ]
    , actual: snd <$> (L.toUnfoldable $ entries trie6)
    }

  let trie7 :: Trie Char String
      trie7 = delete (l $ toCharArray "aabaaaaa") trie6

  assertEqual' "delete #6"
    { expected: []
    , actual: snd <$> (L.toUnfoldable $ entries trie7)
    }

  assertEqual' "delete #6: size"
    { expected: 0
    , actual: size trie7
    }

  assertEqual' "delete #6: isEmpty"
    { expected: true
    , actual: isEmpty trie7
    }

  assertEqual' "delete #6: value"
    { expected: Branch Nothing mempty
    , actual: trie7
    }

  assertEq' "delete #7"
    { expected: mempty
    , actual:
      delete (l [1,2,3,4]) $
      delete (l [1,2,3,4,5]) $
      fromFoldable [ t [1,2,3,4,5] 6
                   , t [1,2,3,4] 5
                   ]
    }

  assertEq' "delete #7: excessive deletions do not harm the trie" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4] 5
                            ] in
      { expected: trie
      , actual:
        delete (l [1,2,3]) $
        delete (l [1,2]) $
        trie
      }

  assertEqual' "delete #8: deletions do not remove longer arcs" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               ]
      , actual:
        delete (l [1,2,3]) $
        trie
      }

  assertEqual' "delete #9: deletions do not remove longer arcs" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,5,6] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [1,2,3,4,6] 5
                               ]
      , actual:
        delete (l [1,2,3,5,6]) $
        trie
      }

  assertEqual' "delete #10: deletions do not remove longer arcs" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [1,2,3,4,6] 5
                               ]
      , actual:
        delete (l [1,2,3]) $
        trie
      }

  assertEqual' "delete #11: deletions do not remove longer arcs" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3] 5
                            ] in
      { expected: trie
      , actual:
        delete (l []) $
        trie
      }

  assertEqual' "delete #12: deletions do not remove longer arcs" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3] 5
                            ] in
      { expected: trie
      , actual:
        delete (l [2]) $
        trie
      }

  assertEqual' "delete #13: deletions do not remove longer arcs" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3] 5
                            ] in
      { expected: trie
      , actual:
        delete (l [1]) $
        trie
      }

  assertEqual' "delete #14" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected:
        fromFoldable [ t [1,2,3,4,5] 6
                     , t [1,2,3,4,6] 5
                     , t [1,2,3] 5
                     ]
      , actual:
        delete (l [1,2,3,4,7]) $
        trie
      }

  assertEqual' "delete #15" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected:
        fromFoldable [ t [1,2,3,4,5] 6
                     , t [1,2,3,4,6] 5
                     , t [1,2,3,4,7] 5
                     ]
      , actual:
        delete (l [1,2,3]) $
        trie
      }

  assertEq' "delete #16: stuctural equality" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            ] in
     { expected: mempty
     , actual: delete (l [1,2,3,4,5]) trie
     }

testDeleteByPrefix :: Effect Unit
testDeleteByPrefix = do
  assertEq' "deleteByPrefix #0" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected: mempty
      , actual:
        deleteByPrefix (l [1,2,3]) $
        trie
      }

  assertEq' "deleteByPrefix #1" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected: mempty
      , actual:
        deleteByPrefix (l []) $
        trie
      }

  assertEq' "deleteByPrefix #2" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3] 5
                               ]
      , actual:
        deleteByPrefix (l [1,2,3,4]) $
        trie
      }

testAlter :: Effect Unit
testAlter = do

  -- Note: no structural equality here.
  assertEqual' "alter #0: can delete using alter" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [1,2,3,4,6] 5
                               , t [1,2,3,4,7] 5
                               ]
      , actual:
        alter (l [1,2,3]) (const Nothing)
        trie
      }

  assertEq' "alter #1: can delete using alter" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected: delete (l [1,2,3]) trie
      , actual:
        alter (l [1,2,3]) (const Nothing)
        trie
      }

  assertEq' "alter #2: can delete using alter" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected: delete (l [1,2,3,4,5]) trie
      , actual:
        alter (l [1,2,3,4,5]) (const Nothing)
        trie
      }

  assertEqual' "alter #3: can delete using alter" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            , t [1,2,3] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,6] 5
                               , t [1,2,3,4,7] 5
                               , t [1,2,3] 5
                               ]
      , actual:
        alter (l [1,2,3,4,5]) (const Nothing)
        trie
      }

  assertEqual' "alter #4: can insert using alter" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            , t [1,2,3,4,7] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [1,2,3,4,6] 5
                               , t [1,2,3,4,7] 5
                               , t [1,2,3] 5
                               ]
      , actual:
        alter (l [1,2,3]) (const $ Just 5)
        trie
      }

  assertEqual' "alter #5: can insert using alter" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [1,2,3,4,6] 5
                               , t [1,2,3,4,7] 5
                               ]
      , actual:
        alter (l [1,2,3,4,7]) (const $ Just 5)
        trie
      }

  assertEqual' "alter #6: can update using alter" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [1,2,3,4,6] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [1,2,3,4,6] 50
                               ]
      , actual:
        alter (l [1,2,3,4,6]) (map (_ * 10))
        trie
      }

  assertEqual' "alter #7: can update using alter" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [] 50
                               ]
      , actual:
        alter (l []) (map (_ * 10))
        trie
      }

testUpdate :: Effect Unit
testUpdate = do

  assertEqual' "update #0" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [] 50
                               ]
      , actual:
        update (_ * 10) (l [])
        trie
      }

  assertEqual' "update #1" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 60
                               , t [] 5
                               ]
      , actual:
        update (_ * 10) (l [1,2,3,4,5])
        trie
      }

  assertEqual' "update #2" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [] 5
                            ] in
      { expected: fromFoldable [ t [1,2,3,4,5] 6
                               , t [] 5
                               ]
      , actual:
        update (_ * 10) (l [1,2,3,4])
        trie
      }

testValues :: Effect Unit
testValues = do

  assertEqual' "values #0" $
    let trie = fromFoldable [ t [1,2,3,4,5] 6
                            , t [] 5
                            ] in
      { expected: l [5,6]
      , actual: values trie
      }

t :: forall b f a. Foldable f => f a -> b -> Tuple (L.List a) b
t path value = Tuple (L.fromFoldable path) value

l :: forall f. Foldable f => (forall a. f a -> L.List a)
l = L.fromFoldable

tup :: forall a b. a -> b -> Tuple a b
tup = Tuple

branch :: forall a b c. Ord b => Foldable c => c (Tuple b (Trie b a)) -> Trie b a
branch lst = Branch Nothing (M.fromFoldable lst)

single :: forall b a. Ord a => b -> Trie a b
single x = Branch (Just x) mempty

arc :: forall a b c. Foldable a => a c -> Trie c b -> Trie c b
arc lst = let lst' = l lst in
  Arc (L.length lst') lst'

giantArray :: Array (Array Char)
giantArray = map toCharArray
       [ ""
       , "ace"
       , "ace-halogen"
       , "activity-monitor"
       , "aff"
       , "aff-bus"
       , "aff-coroutines"
       , "aff-parallel"
       , "aff-promise"
       , "aff-reattempt"
       , "aff-retry"
       , "aff-throttler"
       , "affjax"
       , "affjax-algebra"
       , "airconsole"
       , "airconsole-controls"
       , "airconsole-view-manager"
       , "alexa"
       , "alphasucc"
       , "amqp"
       , "annoy"
       , "ansi"
       , "argonaut"
       , "argonaut-aeson-generic"
       , "argonaut-codecs"
       , "argonaut-core"
       , "argonaut-generic"
       , "argonaut-generic-codecs"
       , "argonaut-traversals"
       , "array-views"
       , "arraybuffer"
       , "arraybuffer-class"
       , "arraybuffer-types"
       , "arrays"
       , "aspen"
       , "assert"
       , "assertions-deepdiff"
       , "audiograph"
       , "autocomplete"
       , "avar"
       , "aws-lambda-express"
       , "b64"
       , "base-rationals"
       , "base58"
       , "base64"
       , "base64-codec"
       , "basic-auth"
       , "behaviors"
       , "benchmark"
       , "benchotron"
       , "bibimbap"
       , "bifunctors"
       , "big-integer"
       , "bigints"
       , "bignumber"
       , "binary"
       , "binary-integers"
       , "bip39"
       , "birds"
       , "bismuth"
       , "bonsai"
       , "boolean-eq"
       , "boomboom"
       , "bound"
       , "bouzuya-http-method"
       , "bouzuya-http-status-code"
       , "boxes"
       , "bq"
       , "browser-cookies"
       , "browserfeatures"
       , "bucketchain"
       , "bucketchain-basic-auth"
       , "bucketchain-conditional"
       , "bucketchain-cors"
       , "bucketchain-csrf"
       , "bucketchain-header-utils"
       , "bucketchain-health"
       , "bucketchain-history-api-fallback"
       , "bucketchain-logger"
       , "bucketchain-secure"
       , "bucketchain-simple-api"
       , "bucketchain-sslify"
       , "bucketchain-static"
       , "bulma"
       , "byte-codec"
       , "bytestrings"
       , "call-by-name"
       , "canvas"
       , "cast"
       , "catenable-lists"
       , "causal-graphs"
       , "chanpon"
       , "chapagetti"
       , "checked-exceptions"
       , "cheerio"
       , "cherry"
       , "chirashi"
       , "choco-pie"
       , "clappr"
       , "clipboard"
       , "clock"
       , "codec"
       , "codec-argonaut"
       , "coercible"
       , "cofree-react-router"
       , "colehaus-graphs"
       , "colehaus-lattice"
       , "colehaus-properties"
       , "colors"
       , "combinators"
       , "command-exists"
       , "concur-react"
       , "concurrent-queues"
       , "conditional"
       , "config"
       , "config2"
       , "confusables"
       , "consable"
       , "console"
       , "console-lifted"
       , "const"
       , "contravariant"
       , "control"
       , "conveyor"
       , "conveyor-basic-auth"
       , "conveyor-cors"
       , "conveyor-health"
       , "cookie"
       , "coroutines"
       , "creditcard-validation"
       , "crypt-nacl"
       , "crypto"
       , "css"
       , "css-bem"
       , "css-validate"
       , "csv"
       , "cycle-run"
       , "d3"
       , "data-algebrae"
       , "dataframe"
       , "datareify"
       , "date-fns"
       , "datetime"
       , "datetime-iso"
       , "day"
       , "debug"
       , "debugger"
       , "decimals"
       , "decision-theory"
       , "density-codensity"
       , "des"
       , "diff"
       , "difference-containers"
       , "digraph"
       , "dispatcher-react"
       , "distributions"
       , "distributive"
       , "dom-classy"
       , "dom-filereader"
       , "dom-indexed"
       , "dom-parser"
       , "dominator-core"
       , "domparser"
       , "dotenv"
       , "dotlang"
       , "drawing"
       , "dynamic"
       , "easings"
       , "easy-alexa"
       , "easy-ffi"
       , "echarts"
       , "eff"
       , "eff-object"
       , "effect"
       , "either"
       , "ejson"
       , "electron"
       , "elmish"
       , "elmish-html"
       , "email-validate"
       , "emmet"
       , "emo8"
       , "emoji-splitter"
       , "encoding"
       , "enums"
       , "enzyme"
       , "error"
       , "errorcontrol"
       , "errors"
       , "es6-symbols"
       , "eth-core"
       , "ethereum"
       , "ethereum-client"
       , "eulalie"
       , "event"
       , "exceptions"
       , "exists"
       , "exitcodes"
       , "expect-inferred"
       , "express"
       , "express-passport"
       , "externs-check"
       , "fetch"
       , "ffi-props"
       , "ffi-utils"
       , "filterable"
       , "filterables"
       , "firebase"
       , "fixed-points"
       , "fixed-precision"
       , "flame"
       , "flare"
       , "flarecheck"
       , "flaredoc"
       , "flatpickr"
       , "float32"
       , "flow-id"
       , "focus-ui"
       , "foldable-traversable"
       , "folds"
       , "foreign"
       , "foreign-datetime"
       , "foreign-generic"
       , "foreign-lens"
       , "foreign-object"
       , "fork"
       , "form-decoder"
       , "form-urlencoded"
       , "format"
       , "format-nix"
       , "formatters"
       , "formatting"
       , "free"
       , "free-alt"
       , "free-alternative"
       , "free-canvas"
       , "free-group"
       , "free-monadplus"
       , "freeap"
       , "freedom"
       , "freedom-portal"
       , "freedom-router"
       , "freedom-transition"
       , "freedom-virtualized"
       , "freedom-window-resize"
       , "freet"
       , "functions"
       , "functor-vector"
       , "functor1"
       , "functors"
       , "fuzzy"
       , "gametree"
       , "gen"
       , "generic-graphviz"
       , "generics"
       , "generics-rep"
       , "generics-rep-optics"
       , "github-corners"
       , "glitter"
       , "globals"
       , "gomtang-basic"
       , "graphql"
       , "graphs"
       , "graphviz"
       , "group"
       , "gun"
       , "halogen"
       , "halogen-bootstrap"
       , "halogen-bootstrap4"
       , "halogen-css"
       , "halogen-datepicker"
       , "halogen-day-picker"
       , "halogen-echarts"
       , "halogen-leaflet"
       , "halogen-menu"
       , "halogen-proxy"
       , "halogen-select"
       , "halogen-storybook"
       , "halogen-vdom"
       , "halogen-vdom-string-renderer"
       , "handlebars"
       , "handsontable"
       , "hareactive"
       , "has-js-rep"
       , "haskell-iso"
       , "heap"
       , "heterogeneous"
       , "heterogenous"
       , "hoist"
       , "home-run-ball"
       , "homogeneous-objects"
       , "hotteok"
       , "html-parser-halogen"
       , "http"
       , "http-methods"
       , "http-types"
       , "httpure"
       , "httpure-middleware"
       , "hyper"
       , "hyper-sslify"
       , "hyperdrive"
       , "hypertrout"
       , "identity"
       , "identy"
       , "idiomatic-node-buffer"
       , "idiomatic-node-crypto"
       , "idiomatic-node-errors"
       , "idiomatic-node-events"
       , "idiomatic-node-http"
       , "idiomatic-node-process"
       , "idiomatic-node-server"
       , "idiomatic-node-stream"
       , "impur"
       , "incremental-dom"
       , "incremental-functions"
       , "indexed-monad"
       , "indexed-nonempty"
       , "indexeddb"
       , "infinite-lists"
       , "inflection"
       , "information"
       , "inject"
       , "int-53"
       , "integers"
       , "intertwine"
       , "intervals"
       , "intl"
       , "intmap"
       , "invariant"
       , "invertible-syntax"
       , "ipfs-api"
       , "isometric"
       , "iterable"
       , "jack"
       , "jajanmen"
       , "jarilo"
       , "jaws"
       , "jest"
       , "jolly-pong"
       , "jquery"
       , "jquery-fancy"
       , "js-cookie"
       , "js-date"
       , "js-timers"
       , "json-pointer"
       , "json-schema"
       , "jsonrpc"
       , "jtable"
       , "jwt"
       , "kancho"
       , "karma-test-unit"
       , "key-based-diff"
       , "kishimen"
       , "kleene-logic"
       , "kubernetes"
       , "kushiyaki"
       , "lambs"
       , "lattice"
       , "lazy"
       , "lcg"
       , "leaflet-tdammers"
       , "learn"
       , "leibniz"
       , "lenient-html-parser"
       , "lens"
       , "line-reader"
       , "linear-algebra"
       , "lists"
       , "lists-fast"
       , "logging"
       , "logging-journald"
       , "logic"
       , "logoot-core"
       , "longs"
       , "luhncheck"
       , "lumi-components"
       , "lunapark"
       , "machines"
       , "mailgun"
       , "makkori"
       , "maps"
       , "markdown"
       , "markdown-halogen"
       , "markdown-smolder"
       , "materialize"
       , "math"
       , "math-equation"
       , "mathbox"
       , "matrices"
       , "matrix"
       , "matryoshka"
       , "maybe"
       , "media-types"
       , "memoize"
       , "merkle-tree"
       , "metadata"
       , "metajelo"
       , "metrics"
       , "midi"
       , "milkis"
       , "minibench"
       , "minimist"
       , "mkdirp"
       , "mmorph"
       , "mochi"
       , "modular-arithmetic"
       , "modules"
       , "moldy"
       , "monad-control"
       , "monad-logger"
       , "monad-loops"
       , "monad-unlift"
       , "monadic-streams"
       , "money"
       , "monoid"
       , "most"
       , "mote"
       , "mother-monad"
       , "motsunabe"
       , "msgpack"
       , "msgpack-msgpack"
       , "multiset-hashed"
       , "murmur3"
       , "mustache"
       , "mysql"
       , "naporitan"
       , "naturals"
       , "nemo"
       , "neon"
       , "newtype"
       , "node-bcrypt"
       , "node-buffer"
       , "node-child-process"
       , "node-coroutines"
       , "node-datagram"
       , "node-dotenv"
       , "node-fs"
       , "node-fs-aff"
       , "node-he"
       , "node-http"
       , "node-net"
       , "node-openurl"
       , "node-os"
       , "node-path"
       , "node-postgres"
       , "node-process"
       , "node-readable"
       , "node-readline"
       , "node-readline-aff"
       , "node-sqlite3"
       , "node-stream-buffers"
       , "node-streams"
       , "node-telegram-bot-api"
       , "node-url"
       , "node-websocket"
       , "nodemailer"
       , "nonempty"
       , "now"
       , "nullable"
       , "nullable-safe"
       , "numbers"
       , "numerics"
       , "oak"
       , "oak-ajax"
       , "oak-debug"
       , "observable-classy"
       , "ochadzuke"
       , "ohyes"
       , "oidc-crypt-utils"
       , "optional"
       , "options"
       , "optlicative"
       , "optparse"
       , "ordered-collections"
       , "orders"
       , "outwatch"
       , "p5"
       , "pairing"
       , "pairs"
       , "pako"
       , "panda"
       , "parallel"
       , "parseint"
       , "parsers"
       , "parsing"
       , "partial"
       , "partial-isomorphisms"
       , "partial-order"
       , "pathy"
       , "paxl"
       , "periodic"
       , "permutations"
       , "pg"
       , "phantom"
       , "phoenix"
       , "pipe-op"
       , "pipes"
       , "pipes-aff"
       , "plaid-node"
       , "plan"
       , "pointed-list"
       , "polyform"
       , "polyform-validators"
       , "polynomials"
       , "posix-types"
       , "postgresql-client"
       , "pouchdb"
       , "pprint"
       , "pqueue"
       , "precise"
       , "precise-datetime"
       , "prelewd"
       , "prelude"
       , "presto"
       , "prettier"
       , "prettier-printer"
       , "probability"
       , "profunctor"
       , "profunctor-lenses"
       , "promises"
       , "propel"
       , "properties"
       , "proportion"
       , "proxy"
       , "psci-support"
       , "pseudo-random"
       , "puchitomato"
       , "pure-css"
       , "pure-style"
       , "compiler-backend-utilities"
       , "pursuit-lookup"
       , "purveyor"
       , "purview"
       , "pux"
       , "pux-clappr"
       , "pux-echarts"
       , "pux-form"
       , "pux-redux"
       , "pux-router"
       , "pux-smolder-dom"
       , "quantities"
       , "quaternions"
       , "querydsl"
       , "queue"
       , "quick-format"
       , "quickcheck"
       , "quickcheck-combinators"
       , "quickcheck-laws"
       , "quickserve"
       , "quill"
       , "quotient"
       , "radox"
       , "random"
       , "random-words"
       , "rationals"
       , "rave"
       , "react"
       , "react-addons-perf"
       , "react-basic"
       , "react-basic-hooks"
       , "react-basic-native"
       , "react-dnd-basic"
       , "react-dom"
       , "react-event-listener"
       , "react-explore"
       , "react-hocs"
       , "react-ix"
       , "react-material-ui"
       , "react-mui"
       , "react-queue"
       , "react-radox"
       , "react-redox"
       , "react-redux"
       , "react-select-basic"
       , "react-spaces"
       , "react-transition-group"
       , "react-transition-group-2"
       , "reactnative"
       , "read"
       , "record"
       , "record-extra"
       , "record-fold"
       , "record-format"
       , "record-prefix"
       , "record-show"
       , "redis-client"
       , "redis-hotqueue"
       , "redox"
       , "redux-devtools"
       , "redux-saga"
       , "refined"
       , "reflection"
       , "refract"
       , "refs"
       , "refty"
       , "remotedata"
       , "requests"
       , "resource"
       , "restify"
       , "restify-router"
       , "result"
       , "ring-modules"
       , "rout"
       , "routing"
       , "row-extra"
       , "rrb-list"
       , "run"
       , "run-console-experiment"
       , "run-streaming"
       , "rwse-free"
       , "rx"
       , "rx-observable"
       , "rxjs"
       , "rxps"
       , "safe-printf"
       , "safelist"
       , "safely"
       , "sammy"
       , "scannable"
       , "screenfull"
       , "screeps-classy"
       , "scrypt"
       , "sdom"
       , "search"
       , "selection-foldable"
       , "selective"
       , "semigroups"
       , "semirings"
       , "sentry-raven"
       , "sequences"
       , "server-sent-events"
       , "setimmediate"
       , "sets"
       , "sforce-remote-action"
       , "shoronpo"
       , "signal"
       , "signal-loop"
       , "sijidou"
       , "simple-ajax"
       , "simple-child-process"
       , "simple-datetime"
       , "simple-emitter"
       , "simple-json"
       , "simple-json-generics"
       , "simple-json-utils"
       , "simple-jwt"
       , "simple-moment"
       , "simple-parser"
       , "simplecrypto"
       , "sized-matrices"
       , "sized-vectors"
       , "sjcl"
       , "sketch"
       , "slice"
       , "slices"
       , "slug"
       , "smash"
       , "smolder"
       , "smolder-dom"
       , "smolder-idom"
       , "smsapi"
       , "snabbdom"
       , "snail"
       , "sockjs-client"
       , "sockjs-node"
       , "sodium"
       , "sorted-arrays"
       , "soundcloud"
       , "soundfonts"
       , "sparkle"
       , "sparrow"
       , "sparrow-queue"
       , "spec"
       , "spec-discovery"
       , "spec-mocha"
       , "spec-quickcheck"
       , "spec-reporter-xunit"
       , "specular"
       , "split"
       , "spork"
       , "sql"
       , "sql-squared"
       , "sqlite"
       , "ssh2-sftp-client"
       , "st"
       , "stablename"
       , "stack"
       , "stacksafe-function"
       , "static-serve"
       , "statistics"
       , "stats"
       , "stdout"
       , "storable"
       , "string-extra"
       , "string-parsers"
       , "strings"
       , "strings-extra"
       , "stringutils"
       , "strongcheck"
       , "strongcheck-generics"
       , "strongcheck-laws"
       , "stuff"
       , "style"
       , "styled-components"
       , "styled-system"
       , "substructural"
       , "subtlecrypto"
       , "sudoku"
       , "sunde"
       , "svg-parser"
       , "svg-parser-halogen"
       , "svg-parser-smolder"
       , "svgo"
       , "symbols"
       , "symmetric-groups"
       , "systemd-journald"
       , "tables"
       , "tables-parse"
       , "tagged"
       , "tagged-sum"
       , "tailrec"
       , "taylor"
       , "template-strings"
       , "test-unit"
       , "text"
       , "text-encoding"
       , "textcursor"
       , "thermite"
       , "thermite-dom"
       , "these"
       , "timeseries"
       , "toppokki"
       , "tortellini"
       , "transformerless"
       , "transformers"
       , "tree"
       , "trie"
       , "tropical"
       , "trout"
       , "trout-client"
       , "tscompat"
       , "tupc"
       , "tuples"
       , "tuples-native"
       , "turbine"
       , "tweetnacl"
       , "twoset"
       , "type-equality"
       , "type-isequal"
       , "typelevel"
       , "typelevel-eval"
       , "typelevel-prelude"
       , "uint"
       , "uint-instances"
       , "unconsable"
       , "undefinable"
       , "undefined"
       , "unfoldable"
       , "unicode"
       , "unicode-prelude"
       , "unique-lists"
       , "unordered-collections"
       , "unordered-containers"
       , "unorm"
       , "unsafe-coerce"
       , "unsafe-reference"
       , "uport"
       , "uri"
       , "uri-extra"
       , "url-validator"
       , "uuid"
       , "validation"
       , "value-of-information"
       , "var"
       , "variant"
       , "vary"
       , "vault"
       , "vector"
       , "vectorfield"
       , "vega"
       , "verbal-expressions"
       , "versions"
       , "vexflow"
       , "vnm-utility"
       , "vom"
       , "web-clipboard"
       , "web-dom"
       , "web-dom-parser"
       , "web-dom-xpath"
       , "web-events"
       , "web-file"
       , "web-html"
       , "web-socket"
       , "web-storage"
       , "web-touchevents"
       , "web-uievents"
       , "web-urlsearchparams"
       , "web-xhr"
       , "web3"
       , "web3-generator"
       , "webaudio"
       , "webcomponents"
       , "webdriver"
       , "weber"
       , "webgl"
       , "webgl2-raw"
       , "webidl"
       , "websocket-moderate"
       , "websocket-simple"
       , "webstorage"
       , "wechaty"
       , "with-index"
       , "word"
       , "workers"
       , "wrappable"
       , "ws"
       , "xiaomian"
       , "xorshift"
       , "xstream"
       , "yargs"
       , "yarn"
       , "yayamll"
       , "z85"
       , "zeromq"
       , "zeta"
       , "zeta-extra"
       ]

-- | Assert structural equality of tries (Data.Search.Trie.Internal.eq')
assertEq'
  :: forall k v
  .  Ord k
  => Eq v
  => Show k
  => Show v
  => String
  -> { expected :: Trie k v
     , actual :: Trie k v
     }
  -> Effect Unit
assertEq' message { expected, actual } =
  if eq' expected actual
  then pure unit
  else do
    log $ "Assertion failed: " <> message
    log $ "Expected: " <> inspect expected
    log $ "Actual:   " <> inspect actual
    assert false


inspect :: forall k v. Show k => Show v => Trie k v -> String
inspect (Branch mb mp) = "(Branch " <> showKey mb <> " " <> showMap mp <> ")"
    where
      showKey = MB.maybe "" show
      showMap m
       | M.isEmpty m = "{}"
       | otherwise =
         "{ " <>
         (A.intercalate ", " (
             (M.toUnfoldable m <#>
              \(Tuple k v) -> show k <> ": " <> inspect v) :: Array String
             )) <>
         " }"
inspect (Arc len path trie) = "(Arc " <> " " <> show (L.toUnfoldable path :: Array k) <> " " <> inspect trie <> ")"
