module Data.Search.Trie.Internal
       ( Trie(..)
       , Ctx(..)
       , Zipper(..)
       , fromFoldable
       , insert
       , insert'
       , isEmpty
       , lookup
       , query
       , subtrie
       , toUnfoldable
       , toUnfoldable'
       )
where

import Prelude

import Data.Foldable (class Foldable, all, foldl)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Maybe as MB
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (over1)
import Data.Unfoldable (class Unfoldable, fromMaybe)
import Data.Array as A

data Trie k v =
  Branch (Maybe v) (Map k (Trie k v))
  -- Length of arc is saved in the structure to speed up lookups.
  -- `List` was chosen because of better asymptotics of its `drop`
  -- operation, in comparison with `Data.Array.drop`.
  -- The list is always non-empty.
  | Arc Int (List k) (Trie k v)

derive instance eqTrie :: (Eq k, Eq v) => Eq (Trie k v)

instance showTrie :: (Show k, Show v) => Show (Trie k v) where
  show (Branch mb mp) = "(Branch " <> show mb <> " " <> showMap mp <> ")"
    where
      showMap m =
        "{ " <>
        (A.intercalate ", " (
            (M.toUnfoldable m <#>
             \(Tuple k v) -> show k <> ": " <> show v) :: Array _
            )) <>
        " }"

  show (Arc len path trie) = "(Arc " <> show len <> " " <> show path <> " " <> show trie <> ")"

instance semigroupTrie :: Ord k => Semigroup (Trie k v) where
  append a b =
    foldl (\m (Tuple path v) -> insert (L.fromFoldable path) v m) b
    (toUnfoldable' a :: List _)

instance monoidTrie :: Ord k => Monoid (Trie k v) where
  mempty = empty

data Ctx k v = BranchCtx (Maybe v) k (Map k (Trie k v))
             | ArcCtx Int (List k)

data Zipper k v = Zipper (Trie k v) (List (Ctx k v))

fromZipper :: forall k v. Ord k => Zipper k v -> Trie k v
fromZipper (Zipper trie Nil) = trie
fromZipper (Zipper trie (Cons x ctx)) =
  case x, trie of
    BranchCtx mbValue key other, _ ->
      fromZipper (Zipper (Branch mbValue $ M.insert key trie other) ctx)
    ArcCtx len path,             Arc len' path' child ->
      fromZipper (Zipper (Arc (len + len') (path <> path') child) ctx)
    ArcCtx len path,             _ ->
      fromZipper (Zipper (Arc len path trie) ctx)

insert :: forall k v. Ord k => List k -> v -> Trie k v -> Trie k v
insert path value trie = fromZipper (insert' path value (Zipper trie Nil))

insert' :: forall k v. Ord k => List k -> v -> Zipper k v -> Zipper k v
insert' Nil  value (Zipper (Branch mbValue children) ctx) =
  Zipper (Branch (Just value) children) ctx
insert' (head : tail) value (Zipper (Branch mbOldValue children) ctx) =
  case M.lookup head children of
    Just child ->
      insert' tail value $
      Zipper child
             (BranchCtx mbOldValue head children : ctx)
    Nothing ->
      Zipper (mkArc tail $ Branch (Just value) mempty)
             (BranchCtx mbOldValue head children : ctx)
insert' path value (Zipper (Arc len arc child) ctx) =
  let prefixLength = longestCommonPrefixLength path arc in
  if prefixLength == len
  then
    let newPath = L.drop prefixLength path in
    insert' newPath value $
    Zipper child $
    ArcCtx len arc : ctx
  else
    if prefixLength == 0 then
      -- Replace `Arc` with a `Branch`.
      case L.uncons arc of
        Just { head, tail } ->
          -- We want to avoid `L.length` call on `tail`: at this point
          -- the length can be calculated.
          let len' = len - 1
              children = M.singleton head $
                         if len' > 0
                         then Arc len' tail child
                         else child
          in
            insert' path value $
            Zipper (Branch Nothing children) ctx
        Nothing ->
          Zipper empty ctx -- impossible: `arc` is always non-empty
    else
      let
        outerArc = L.take prefixLength path
        newPath  = L.drop prefixLength path
        -- `innerArc` is always non-empty, because
        -- `prefixLength == L.length arc` is false in this branch.
        -- `prefixLength <= L.length arc` is true because `prefixLength` is
        -- a length of some prefix of `arc`.
        -- Thus `prefixLength < L.length arc`.
        innerArc = L.drop prefixLength arc
        innerArcLength = len - prefixLength
        outerArcLength = L.length outerArc
      in
        insert' newPath value $
        Zipper (Arc innerArcLength innerArc child)
        if outerArcLength == 0
        then ctx
        else ArcCtx outerArcLength outerArc : ctx

-- | A smart constructor to ensure Arc non-emptiness.
mkArc :: forall k v. List k -> Trie k v -> Trie k v
mkArc Nil trie = trie
mkArc arc trie = Arc (L.length arc) arc trie

empty :: forall k v. Ord k => Trie k v
empty = Branch Nothing mempty

isEmpty :: forall k v. Trie k v -> Boolean
isEmpty (Branch (Just _) _) = false
isEmpty (Branch Nothing children) =
  all (snd >>> isEmpty) (M.toUnfoldableUnordered children :: Array (Tuple k (Trie k v)))
isEmpty (Arc _ _ child) =
  isEmpty child

subtrie :: forall k v. Ord k => List k -> Trie k v -> Maybe (Trie k v)
subtrie path (Arc len arc child) =
  let prefixLength = longestCommonPrefixLength path arc in
  if prefixLength == len
  then
    subtrie (L.drop prefixLength path) child
  else
    Just $ mkArc (L.drop prefixLength arc) child
subtrie path trie@(Branch _ children) =
  case L.uncons path of
    Nothing -> Just trie
    Just { head, tail } ->
      case M.lookup head children of
        Just trie' -> subtrie tail trie'
        Nothing -> Nothing

lookup :: forall k v. Ord k => List k -> Trie k v -> Maybe v
lookup path trie =
  subtrie path trie >>= case _ of
    Branch mbValue _ -> mbValue
    _                -> Nothing

query
  :: forall p l k v
  .  Ord k
  => Functor l
  => Unfoldable l
  => Unfoldable p
  => Trie k v
  -> List k
  -> l (Tuple (p k) v)
query trie path =
  toUnfoldable $ MB.fromMaybe empty $ subtrie path trie

longestCommonPrefixLength :: forall a. Eq a => List a -> List a -> Int
longestCommonPrefixLength = go 0
  where
    go n xs ys =
      case L.uncons xs, L.uncons ys of
        Just x, Just y ->
          if x.head == y.head
          then go (n + 1) x.tail y.tail
          else n
        _, _ -> n

fromFoldable
 :: forall f p k v
 .  Ord k
 => Foldable f
 => Foldable p
 => f (Tuple (p k) v)
 -> Trie k v
fromFoldable =
  foldl (\m (Tuple path v) -> insert (L.fromFoldable path) v m) empty

toUnfoldable
  :: forall f p k v
  .  Unfoldable f
  => Unfoldable p
  => Trie k v
  -> f (Tuple (p k) v)
toUnfoldable trie =
  L.toUnfoldable (toUnfoldable' trie <#> over1 L.toUnfoldable)

toUnfoldable'
  :: forall k v
  .  Trie k v
  -> List (Tuple (List k) v)
toUnfoldable' (Branch mbValue children) =
  let valueList =
        fromMaybe mbValue <#> Tuple Nil
      childrenList =
        M.toUnfoldable children >>=
        (\(Tuple char trie) ->
          over1 (char : _) <$> toUnfoldable' trie)
  in valueList <> childrenList
toUnfoldable' (Arc len path child) =
  over1 (path <> _) <$> toUnfoldable' child
