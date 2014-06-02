module CLikeVisitor where

import CLikeTypes
import Data.Maybe
import Data.String.Utils

-- Does pure mapping. Each function call is 100% the same.

-- iterateWithParent :: (Node -> Node -> IO () ) -> Node -> IO()
-- iterateWithParent f root =
--     let visitThis = map (f root) (children root)
--         visitRest = map (iterateWithParent f) (children root)
--     in do
--         visitThis
--         visitRest

-- printVisitor :: Node -> Node -> IO ()
-- printVisitor parent node = do
--     putStrLn $ (show nodeId) ++ "\t" ++ kind node
--     putStrLn $ (show nodeId) ++ (map (++ " " . print) (children node))

-- Map over all the nodes,
-- a    some attribute for each node. For the printer, this is the node's ID
listAssignAccum :: (a->a) -> [((Node, a), [(Node, a)])] -> Node -> [((Node, a), [(Node, a)])]
listAssignAccum incA nodeList parent =
    let assignedChildren
            = foldl
                (assignChildAs incA)
                -- Sneaky/hacky trick.. Immediately grab the first child and its ID as the starting point, binding the next available ID into the starting value
                -- TODO: Still need to allow for no-children nodes
                [(head $ children parent, incA $ getNextAvailableA incA nodeList)]
                (tail $ children parent)
        thisParent = ((parent, getNextAvailableA incA nodeList), assignedChildren)
        -- withSelf = prevNodeList ++ [(parent,child, incA $ third $ last prevNodeList)]
        includingThisParent = thisParent:nodeList
    in -- accumChildren incA child withSelf
        -- assignedChildren assigned IDs to the children
        -- thisParent assigned the ID to the parent, and then paired it with its children and their IDs
        -- Now, recursion must happen to accumulate all the parent-children sets
        -- Use this function as a folder across the descension into children, connecting all the recursions together
        foldl (listAssignAccum incA)
            includingThisParent
            (children parent)

-- folding function to match a parent's children with their As
assignChildAs :: (a->a) -> [(Node,a)] -> Node -> [(Node,a)]
assignChildAs incA all@((_,lastA):rest) child = (child, incA lastA):all

-- returns the next A. This is SAFE for empty children lists
getNextAvailableA :: (a->a) -> [((Node,a), [(Node,a)])] -> a
getNextAvailableA incA (lastEntry@(parentPair, childList):rest) =
    if length childList == 0 then incA $ snd parentPair
    else incA $ snd $ last childList

assignAsToRoot :: (a->a) -> a -> Node -> [((Node, a), [(Node, a)])]
assignAsToRoot incA startA root =
    listAssignAccum incA [((root, startA),[])] (head $ children root)

mapProducer :: ( (Node,a)->Maybe [(Node,a)] -> b ) -> [((Node,a), [(Node,a)])] -> [b]
mapProducer prodFunc pairList =
    map (applyProducer prodFunc) pairList

applyProducer :: ( (Node,a)->Maybe [(Node,a)] -> b ) -> ((Node,a), [(Node,a)]) -> b
applyProducer prodFunc (parentPair, childrenPairs) =
    if length childrenPairs == 0 then prodFunc parentPair Nothing
    else prodFunc parentPair $ Just childrenPairs

runPrinter :: Node -> IO()
runPrinter root = foldl doAccum (return()) $ mapProducer nodePrinter $ assignAsToRoot (1+) 0 root

doAccum :: IO() -> IO() -> IO()
doAccum a b = do
    a
    b

nodePrinter :: Show a => (Node, a) -> Maybe [(Node, a)] -> IO()
-- Must be the root node
nodePrinter p@(pnode, pid) children = do
    putStrLn $ (show pid) ++ " " ++ (show $ nodeKind pnode) ++ if isNothing $ nodeData pnode then "" else " " ++ (show $ fromJust $ nodeData pnode )
    if isJust children then
        putStrLn $ (show pid) ++ " " ++ (join " " (map (show . snd) (fromJust children)))
    else return ()
