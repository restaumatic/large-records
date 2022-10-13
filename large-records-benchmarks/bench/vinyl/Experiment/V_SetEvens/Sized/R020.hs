#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module Experiment.V_SetEvens.Sized.R020 where

import Common.Record
import Bench.EvensOfSize.Evens020
import Common.RowOfSize.Row020

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
    $ r
