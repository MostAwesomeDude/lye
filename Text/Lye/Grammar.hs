import Text.Parsers.Frisby
import Text.Parsers.Frisby.Char

type Pitch = Int
type Duration = Int

data Marker = Relative
    deriving (Eq, Show)

data Note = Note Pitch Duration
    deriving (Eq, Show)

type Chord = [Note]

-- | Parse a string into an arbitrary object, consuming any leading
-- | whitespace.
token :: String -> a -> PM s (P s a)
token s m = newRule $ optional (many space) ->> (text s ##> m)

