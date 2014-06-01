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
listAssignAccum :: (a->a) -> Node -> [(Node, Node, a)] -> Node -> [(Node, Node, a)]
listAssignAccum incA parent prevNodeList child =
    let withSelf = prevNodeList ++ [(parent,child, incA $ third $ last prevNodeList)]
    in accumChildren incA child withSelf

accumChildren :: (a->a) -> Node -> [(Node,Node,a)] -> [(Node,Node,a)]
accumChildren incA parent nodeList
    = foldl (listAssignAccum incA parent) nodeList (children parent)

printVisitor :: Node -> IO ()
printVisitor parent =
    map applyPrinter $ accumChildren (1+) parent [(parent, head $ children parent, 0)]

third :: (a,b,c) -> c
third (_,_,c) = c

applyPrinter :: [(Node, Node, a)] -> IO()

nodePrinter :: Show a => (Node, a) -> Maybe [(Node, a)] -> IO()
-- Must be the root node
nodePrinter p@(pnode, pid) children = do
    putStrLn $ (show pid) ++ " " ++ (show $ nodeKind pnode) ++ if isNothing $ nodeData pnode then "" else " " ++ (show $ fromJust $ nodeData pnode )
    if isJust children then
        putStrLn $ (show pid) ++ " " ++ (join " " (map (show . snd) (fromJust children)))
    else return ()
