{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
module Common.Record where

import Data.Kind (Constraint, Type)
import Control.Lens (Iso, iso)
import Control.Lens.TH (makeWrapped)
import Control.DeepSeq(NFData(rnf))
import Data.Vinyl (Rec((:&), RNil))
import qualified Data.Vinyl as V
import qualified Data.Vinyl.TypeLevel as V
import Data.Functor.Identity (Identity(Identity))
import Data.Vinyl.TypeLevel (Nat (S, Z), type (++))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl.XRec(IsoHKD(HKD, toHKD, unHKD))
import GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)
import GHC.OverloadedLabels (IsLabel, fromLabel)

type Field = Type

type Record = Rec Identity

newtype (:->) (s :: Symbol) a = Val { getVal :: a }

_Val :: Iso (s :-> a) (s :-> b) a b
_Val = iso getVal Val

makeWrapped ''(:->)

(=:) :: forall label a. Label label -> a -> Record '[label :-> a]
Label =: value = pure (Val value) :& RNil

instance NFData a => NFData (s :-> a) where
  rnf (Val x) = rnf x

instance NFData (Record '[]) where
  rnf RNil = ()

instance (NFData x, NFData (Record xs)) => NFData (Record (x : xs)) where
  rnf (x :& xs) = rnf x `seq` rnf xs


instance forall (s :: Symbol) a. (KnownSymbol s, Show a) => Show (s :-> a) where
  showsPrec p (Val a) = ((symbolVal (Proxy :: Proxy s) ++ " :-> ") ++) . showsPrec p a

instance KnownSymbol s => IsoHKD Identity (s :-> a) where
  type HKD Identity (s :-> a) = a
  unHKD = Identity . Val
  toHKD (Identity (Val x)) = x


val :: forall (s :: Symbol) a. a -> Identity (s :-> a)
val = Identity . Val @s

(<+>) :: Record xs -> Record ys -> Record (xs ++ ys)
(<+>) = (V.<+>)

infixr 5 <+>

rnil :: Record '[]
rnil = RNil

--
type family The k (a :: k) :: k where
  The k a = a

-- | Just a tag to make ('~>') work.
data TyFun a b

{- | Defunctionalized type level function.
 Borrowed from @singletons@.
 See <http://hackage.haskell.org/package/singletons-2.2/docs/Data-Singletons.html#t:-126--62->
-}
type a ~> b = TyFun a b -> Type

-- | Apply a defunctionalized function.
type family (f :: a ~> b) $$ (x :: a) :: b

-- | Convert a data constructor to a ('~>').
data TyCon1 (c :: a -> b) :: a ~> b

type instance TyCon1 c $$ a = c a

type family Map (fn :: (a :: k) ~> (b :: k)) (xs :: (f :: k -> Type) a) :: f b
type instance Map fn (Nothing :: Maybe k) = The (Maybe k) Nothing
type instance Map fn (Just x) = Just (fn $$ x)

{- | Change the element at the given index in a list.

 >>> :kind! SetAt (V.S (V.S V.Z)) 10 '[1,2,3,4,5,6]
 SetAt (V.S (V.S V.Z)) 10 '[1,2,3,4,5,6] :: [Nat]
 = '[1, 2, 10, 4, 5, 6]
-}
type family SetAt (index :: V.Nat) (x :: k) (xs :: [k]) :: [k] where
  SetAt V.Z x (y ': xs) = x ': xs
  SetAt (V.S n) x (y ': xs) = y ': SetAt n x xs


type family FromMaybeTypeError (err :: ErrorMessage) (x :: Maybe a) :: a where
  FromMaybeTypeError err Nothing = TypeError err
  FromMaybeTypeError err (Just x) = x

type family RIndexKey (key :: Symbol) (xs :: [Field]) :: Nat where
  RIndexKey key xs =
    FromMaybeTypeError
      ( Text "Record label " :<>: ShowType key :<>: Text " not found in the record " :$$: ShowType xs
      )
      (RIndexKeyMaybe key xs)

type family RIndexKeyMaybe (key :: Symbol) (xs :: [Field]) :: Maybe Nat where
  RIndexKeyMaybe key '[] = Nothing
  RIndexKeyMaybe key ((key :-> value) ': xs) = Just Z
  RIndexKeyMaybe key ((other :-> value) ': xs) = Map (TyCon1 S) (RIndexKeyMaybe key xs)

type family RLabelValue (key :: Symbol) (xs :: [Field]) :: Type where
  RLabelValue key ((key :-> value) ': xs) = value
  RLabelValue key ((other :-> value) ': xs) = RLabelValue key xs

type HasKey (key :: Symbol) (rs :: [Field]) (a :: Type) =
  ( KnownSymbol key
  , V.RElem (key :-> a) rs (RIndexKey key rs)
  , a ~ RLabelValue key rs
  )

-- | A singleton for record labels.
data Label (label :: Symbol) where
  Label :: KnownSymbol label => Label label

instance (label ~ label', KnownSymbol label) => IsLabel label' (Label label) where
  fromLabel = Label

labelVal :: Label label -> String
labelVal label@Label = symbolVal label


class RPutPoly (rs :: [Field]) (i :: Nat) a where
  rputPoly :: Proxy i -> a -> Record rs -> Record (SetAt i a rs)

instance RPutPoly (r ': rs) Z a where
  rputPoly _ x (Identity _ :& rs) = Identity x :& rs

instance
  (RPutPoly rs n a, SetAt ( 'S n) a (r : rs) ~ (r : SetAt n a rs)) =>
  RPutPoly (r ': rs) (S n) a
  where
  rputPoly _ x (y :& rs) = y :& rputPoly (Proxy @n) x rs

type RPut key a b rs ss =
  ( KnownSymbol key
  , HasKey key rs a
  , RPutPoly rs (RIndexKey key rs) (key :-> b)
  , ss ~ SetAt (RIndexKey key rs) (key :-> b) rs
  )

get ::
  forall key rs a.
  (HasKey key rs a) =>
  Record rs ->
  a
get r = case V.rget @(key :-> a) r of Identity (Val x) -> x

put ::
  forall key b a rs ss.
  (RPut key a b rs ss) =>
  b ->
  Record rs ->
  Record ss
put x = rputPoly (Proxy @(RIndexKey key rs)) (Val x :: key :-> b)
