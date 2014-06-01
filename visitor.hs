module CLikeVisitor where

import CLikeTypes

-- Does pure mapping. Each function call is 100% the same.

iterateWithParent :: (Node -> Node -> IO () ) -> Node -> IO()
iterateWithParent f root =
    let visitThis = map (f root) (children root)
        visitRest = map (iterateWithParent f) (children root)
    in do
        visitThis
        visitRest

printVisitor :: Node -> Node -> IO ()
printVisitor parent node = do
    putStrLn $ (show nodeId) ++ "\t" ++ kind node
    putStrLn $ (show nodeId) ++ (map (++ " " . print) (children node))

-- But what if I want to accumulate a node ID as nodes are visited?

-- Takes a similar set of stuff as before, except we use a as the intermediate data, and use a step function
-- foldl :: (b -> a -> b) -> b -> [a] -> b
foldWithParent :: ( a -> Node -> Node -> (a, IO()) ) (a -> a) -> a -> Node -> (a, IO())
foldWithParent nodeproc incA parenta parent =
    let postProcChildren = foldl
        (children node)
    in do
        visitChildren

printFold :: Node -> IO
printFold rootNode =
    snd $ foldl
        (foldingFunc \(a->a+1) \(nodePrinter) Nothing )
        (0, IO())
        (children rootNode)

-- This function needs to first be curried with the parent (first arg), and THEN it can be folded across children
foldingFunc :: (a -> a) -> (Maybe Node -> Node -> IO() ) -> Node -> (a, IO) -> Node -> (a, IO)
foldingFunc incA processNode parent (childa, ioaccum) child =
    let nextA = incA childa
        -- Either only proc this child, or bind it with processing the child's children as well
        procThisChild = processNode parent child

        -- Recurses through the children only if there are children (lazy eval)
        processChildren = foldl (foldingFunc incA processNode child) (nextA, procThisChild) (children child)
    in if length $ children child > 0
        then processChildren
        else (nextA, procThisChild)

-- All that was WRONG.
-- match each child with an id, and pass them both into the printer
--
-- map . fold . children
-- first fold
-- then map
--
-- For the printer, the foldl will be ([(Node,Int)] -> Node -> [(Node,Int)]) or [child and intID] -> nextChild -> [child and intID] where intIDs are accumulated and linked with children
mapFoldNode :: ([ -> Node -> a)     -- folding function. a is the result of the map over children, and b is the accumulator
            -> a    -- initial value for the fold
            -> ((Node,b) -> Maybe [(Node,b)] -> c)       -- mapping function. for printing, this is covered by the nodePrinter
            -> Node
            -> (c -> c -> c)     -- Combiner for the mapping function. It'll get foldled across the c's, taking 2 c's, and turning them into 1. For printing, c is an IO monad.
            -> (b, c)    -- End result is the last b from the fold (so it can be used to recursively fold, and the last c from all the operations
mapFoldNode foldFunc initFold mapFunc parentNode
    -- What I need:
    -- starting a (for printVisitor, this is 0::Int)
    -- a -> a to do incrementation
    let 
        -- Each child needs to be assigned an ID. This fold does that
        -- It also recurses onto children, since their IDs need to be contiguous
        idAssignNodes = foldl
            \( (lastNode:restNodes)->child-> 
                -- This node with ID
                (child,incA $ snd $ lastNode)
                -- Recurse assignment
                : 
                ++restNodes )
            [(first $ children parent, incA a)]
            (tail $ children parent)
        -- The next a needs to come from the last one after the ones assigned to children
        postChildA = incA $ snd $ last idAssignNodes

        -- Run the mapper with the parent across the children
        mapped = mapFunc (parent, a)

------
-- Another organization issue: Things need to be connected so that ids are assigned as recursion happens
------

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
    accumChildren \(i->i+1) parent [(parent, first $ children parent, 0)]

nodePrinter :: (Node, a) -> Maybe [(Node, a)] -> IO()
-- Must be the root node
nodePrinter p@(pnode, pid) children = do
    putStrLn (show pid) ++ " " ++ (show $ nodeKind p) ++ (show $ if isNothing $ nodeData p then "" else " " ++ show $ fromJust $ nodeData p )
    if isJust children then
        putStrLn (show pid) ++ " " ++ (join " " (map (show . snd) (fromJust children)))
    else return ()
