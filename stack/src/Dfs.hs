module Dfs where

import Control.Monad.State

-- dfs (v):
--   visited(v) = 1
--   for (u: r[v])
--      if (not visited[u])
--          dfs (u)

--
--
--
--

type Vertex = Int
type Graph = [[Vertex]]

dfs :: Vertex -> Graph -> [Vertex]
dfs v g = execState (dfs' v g) [] where
  dfs' :: Vertex -> Graph -> State [Vertex] ()
  dfs' v g = do
        visited <- get
        let l = v:visited
        put l
        mapM (\x -> do
          when (not $ x `elem` l) (dfs' x g)) l
