#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif

{-# LANGUAGE OverloadedLabels #-}

module Experiment.V_UpdateOne.Sized.R050 where

import Common.Record
import Bench.Types
import Common.RowOfSize.Row050

updateOne :: Record ExampleRow -> Record ExampleRow
updateOne = put @"t00" (MkT 0)
