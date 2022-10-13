#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module Experiment.V_SetEvens.Sized.R060 where

import Common.Record

import Bench.EvensOfSize.Evens060
import Common.RowOfSize.Row060

setEvens :: Evens -> Record ExampleRow -> Record ExampleRow
setEvens Evens{..} r =
      -- 00 .. 09
      put @"t00" evens00
    . put @"t02" evens02
    . put @"t04" evens04
    . put @"t06" evens06
    . put @"t08" evens08
      -- 10 .. 19
    . put @"t10" evens10
    . put @"t12" evens12
    . put @"t14" evens14
    . put @"t16" evens16
    . put @"t18" evens18
      -- 20 .. 29
    . put @"t20" evens20
    . put @"t22" evens22
    . put @"t24" evens24
    . put @"t26" evens26
    . put @"t28" evens28
      -- 30 .. 39
    . put @"t30" evens30
    . put @"t32" evens32
    . put @"t34" evens34
    . put @"t36" evens36
    . put @"t38" evens38
      -- 40 .. 49
    . put @"t40" evens40
    . put @"t42" evens42
    . put @"t44" evens44
    . put @"t46" evens46
    . put @"t48" evens48
      -- 50 .. 59
    . put @"t50" evens50
    . put @"t52" evens52
    . put @"t54" evens54
    . put @"t56" evens56
    . put @"t58" evens58
    $ r
