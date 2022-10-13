#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module Experiment.V_SetEvens.Sized.R010 where

import Common.Record

import Bench.EvensOfSize.Evens010
import Common.RowOfSize.Row010

setEvens :: Evens -> Record ExampleRow -> Record ExampleRow
setEvens Evens{..} r =
      -- 00 .. 09
      put @"t00" evens00
    . put @"t02" evens02
    . put @"t04" evens04
    . put @"t06" evens06
    . put @"t08" evens08
    $ r
