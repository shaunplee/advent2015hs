{-# LANGUAGE OverloadedStrings #-}

module GetInput where

import           Control.Lens
import           Data.Aeson      (Value)
import           Data.Aeson.Lens (key, _String)
import           Data.Map        as Map
import           Network.Wreq
