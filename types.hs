module CLikeTypes where

data Node = Node {
      nodeKind :: NodeKind
    , nodeData :: NodeDataType
    , children :: [Node]
    , parent   :: Node
} deriving Show

newtype NodeDataType = Maybe ( String | Int )

type NodeKind =
      Term
    | Part
    | Type
    deriving Show, Enum

mkFamily :: 

-- data Program
newtype Type = Type [String]
    deriving Show

modifyType :: String -> Type -> Type
modifyType s (Type v) = Type (s:v)

data Term =
      TermValue Part
    | TermMult  Part Part
    | TermDiv   Part Part
    | TermLSh   Part Part
    | TermRSh   Part Part
    deriving Show

data Part =
      PartID String
    | PartNum Int
    deriving Show
