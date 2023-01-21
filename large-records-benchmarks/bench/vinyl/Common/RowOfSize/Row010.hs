{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}


module Common.RowOfSize.Row010 (ExampleRow) where

import Common.Record
import Bench.Types

type ExampleRow = [
    -- 00 .. 09
    "t00" :-> T 00
  , "t01" :-> T 01
  , "t02" :-> T 02
  , "t03" :-> T 03
  , "t04" :-> T 04
  , "t05" :-> T 05
  , "t06" :-> T 06
  , "t07" :-> T 07
  , "t08" :-> T 08
  , "t09" :-> T 09
  ]
