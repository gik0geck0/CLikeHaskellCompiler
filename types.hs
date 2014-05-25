module CLikeTypes where

data Node = Node {
      nodeKind :: NodeKind
    , nodeData :: Maybe NodeDataType
    , children :: [Node]
    , parent   :: Maybe Node
} deriving Show

data NodeDataType =
      StringData String
    | IntegerData Int
    deriving Show


data NodeKind =
      Variable      -- Data is a String. used on the Left  of an =
    | Identifier    -- Data is a String. used on the Right of an =
    | Number

    | DeclarationStatement

    | DeclareVariable
    | DeclareVariableMulti
    | InstantiateVariable
    | InstantiateVariableMulti

    | Type          -- Data is a String with either a type or a type modifier
    | TypeModifier
    | TypeModifierMulti
    | TypeConstructor

    | TermValue
    | OperationMult
    | OperationDiv
    | OperationLSh
    | OperationRSh
    | OperationPlus
    | OperationMinus
    | BooleanOperationLThan
    | BooleanOperationLEThan
    | BooleanOperationGThan
    | BooleanOperationGEThan
    | BooleanOperationDoubleEquals
    deriving (Show, Enum)

-- Creates a new node of a certain kind, and instantiates with a list of children
mkFamily :: NodeKind -> [Node] -> Node
mkFamily nkind children = Node nkind Nothing children Nothing

-- Create a node with a kind, some data, no children and no parent
mkNode :: NodeKind -> Maybe NodeDataType -> Node
mkNode nkind ndata = Node nkind ndata [] Nothing

-- add more children to a node
adoptChildren :: Node -> [Node] -> Node
adoptChildren parent@(Node pk pd pc pp) children = _updateParent $ Node pk pd (pc ++ children) pp

-- Returns the same node passed in except with a different parent
_changeParent :: Maybe Node -> Node -> Node
_changeParent parent child@(Node k d c _) = Node k d c parent

-- Makes the children of this node have their parent set to itself
--
-- TODO: How do I make a new Node with its children pointing to it?
-- How can you get a reference to a yet uncreated Node, so it can
-- be used for the childrens' parent?
--
-- TODO: this node's parent's children will not contain this newly created node
-- Do I need to using lens or something to make this work?
-- During construction only (bottom-up construction), this is not a problem
--
-- TODO: Currently, the parent is None. Always
_updateParent :: Node -> Node
_updateParent parent@(Node pk pd children pp) = Node pk pd (map (_changeParent Nothing) children) pp


-- This used to be used to identify the type using a single node. Now, its spread out in the AST
-- data Type = Type [String]
--    deriving Show
--
-- modifyType :: String -> Type -> Type
-- modifyType s (Type v) = Type (s:v)

-- data Term =
--       TermValue Part
--     | TermMult  Part Part
--     | TermDiv   Part Part
--     | TermLSh   Part Part
--     | TermRSh   Part Part
--     deriving Show
-- 
-- data Part =
--       PartID String
--     | PartNum Int
--     deriving Show
