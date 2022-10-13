#if PROFILE_CORESIZE
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl #-}
#endif
#if PROFILE_TIMING
{-# OPTIONS_GHC -ddump-to-file -ddump-timings #-}
#endif
{-# LANGUAGE OverloadedLabels #-}

module Experiment.V_Construct.Sized.R010 where

import Common.Record
import Bench.Types
import Common.RowOfSize.Row010 (ExampleRow)

record :: Word -> Record ExampleRow
record x =
      -- 00 .. 09
       (#t00 =: MkT x)
   <+> (#t01 =: MkT x)
   <+> (#t02 =: MkT x)
   <+> (#t03 =: MkT x)
   <+> (#t04 =: MkT x)
   <+> (#t05 =: MkT x)
   <+> (#t06 =: MkT x)
   <+> (#t07 =: MkT x)
   <+> (#t08 =: MkT x)
   <+> (#t09 =: MkT x)
   <+> rnil
