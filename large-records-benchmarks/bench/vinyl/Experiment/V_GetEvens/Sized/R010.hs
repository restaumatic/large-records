#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
module Experiment.V_GetEvens.Sized.R010 where

import Common.Record (get, Record)
import Bench.EvensOfSize.Evens010
import Common.RowOfSize.Row010

getEvens :: Record ExampleRow -> Evens
getEvens r = Evens {
      -- 00 .. 09
      evens00 = get @"t00" r
    , evens02 = get @"t02" r
    , evens04 = get @"t04" r
    , evens06 = get @"t06" r
    , evens08 = get @"t08" r
    }
