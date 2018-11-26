{-# LANGUAGE TypeFamilies        #-}
-- |
module Komposition.Application.Form where

data Valid a

type family FormData f t where
  FormData Valid t = t
  FormData f t = f t
