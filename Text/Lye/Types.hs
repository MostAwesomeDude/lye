module Text.Lye.Types where

type Pitch = Int

type Duration = Int

data Note = Note Pitch Duration
          | Rest Duration
    deriving (Eq, Show)
