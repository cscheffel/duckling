-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.ApprenticeYear.DE.Corpus
  ( corpus ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.ApprenticeYear.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale DE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Lehrjahr 1)
             [ "1 Lehrjahr"
              , "1. Lehrjahr"
             --, "ein liter"
             ]
  , examples (simple Lehrjahr 2)
             [ "2 Lehrjahr"
             , "2. Lehrjahr"
             ]
  , examples (simple Lehrjahr 1)
             [ "erstes Lehrjahr"
             , "erstes Ausbildungsjahr"
             ]
  , examples (simple Lehrjahr 3)
             [ "drittes Ausbildungsjahr"
             , "drittes Lehrjahr"
             ]
  ]
