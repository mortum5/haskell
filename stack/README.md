Haskell
========

Function
--------

zip :: [a] -> [b] -> [(a, b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

fmap :: (a -> b) -> f a -> f b
< * > :: f (a -> b) -> f a -> f b

IORef
-----

```Haskell
main = do
    var <- newIORef 2
    x <- readIORef var
    print x
    writeIORef var 4
    x <- readIORef var
    print x
    ```

#### Data.Pool

#### Data.Aeson

#### Database.Bolt

#### ActionT

#### STRef
Similar IORef but use State Monad instead of IO

#### WriterT

(runWriter . runWriterT ) $ separate (<3) (>7) [0..10]
(([3,4,5,6,7],[0,1,2]),[8,9,10])
????

#### MVar
Similar to IORef but can be empty

We have two ways of constructing an MVar.
```Haskell
newMVar :: a -> IO (MVar a)
newEmptyMVar :: IO (MVar a)
```
We also have an additional operation ```haskell takeMVar :: MVar a -> IO a``` which takes a value out of an MVar and leaves it empty. Now comes the important part, if we try to do takeMVar from an empty MVar, it will block the thread until someone else puts a value into the MVar. The same thing happens when you try to putMVar into an MVar that already has a value, it will block until someone takes that value out.

One of the great benefits of MVars is that they can be be used to serve as synchronization primitives for communication between threads.

We can use them as a simple 1 item channel, where we fork a thread that forever loops trying to read from the MVar and print the result, and in the main thread we read input from the user and put it into the same MVar.

#### Software Transactional Memory - STM

#### Functor
TODO законы
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f xs = do {x < - xs; return (f x)}

#### Applicative

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap fs xs = do {f < - fs; x < - xs; return (f x)}

#### Monad
http://slides.com/fp-ctd/lecture-9#/33

#### Vector
```Haskell
import qualified Data.Vector as V

a = V.fromList [1,2,3,4]
V.head a
V.enumFromN 10 4
V.foldl
V.map
V.!
```

2.2.1 Boxed Arrays: Data.Vector
The most flexible type is Data.Vector.Vector, which provides *boxed* arrays: arrays of pointers to Haskell values.

Data.Vector.Vector's are fully polymorphic: they can hold any valid Haskell type
These arrays are suitable for storing complex Haskell types (sum types, or algebraic data types), but a better choice for simple data types is Data.Vector.Unboxed.

2.2.2 Unboxed Arrays: Data.Vector.Unboxed
Simple, atomic types, and pair types can be stored in a more efficient manner: consecutive memory slots without pointers. The Data.Vector.Unboxed.Vector type provides unboxed arrays of types that are members of the Unbox class, including:

#### Cont CPS

square :: Int -> (Int - r) -> r

square 2 square id

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

#### Applicative and MonadPlus
m >>= return . f  we can replace with Applicative
anyChr :: Prs Char

msum asum

Can we?

anyChr :: Parser Char

fmap fmap fmap == fmap . fmap (for reader)

#### ReaderM hidding

#### MultiParamTypeClasses
Позволяет определять классы типов с несколькими параметрами
class a b c where
( *** ) :: a->b->c
Но, возникает проблема
instance Mull Int Double Double where
...
если мы явно не укажем тип x *** y, то компилятор скажет, что не может определить для этого введём функциональную зависимость
{-# LANGUAGE FunctionalDependencies #-}
class a b c | a b -> c where
Теперь тип c однозначно определяется по a b

#### Exception

Можно добавить MonadPlus, чтобы можно было пробовать цепочку вычислений по очереди, до первого удачного, в противном случае накапливать ошибки
The bracket function comes from the Control.Exception module. It helps perform actions safely.

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

#### Vector

However, that's not the only dimension of choice you get in the vector package. vector itself defines three flavors: boxed (Data.Vector/Data.Vector.Mutable), storable (Data.Vector.Storable and Data.Vector.Storable.Mutable), and unboxed (Data.Vector.Unboxed and Data.Vector.Unboxed.Mutable)

In general:

    End users should use Data.Vector.Unboxed for most cases
    If you need to store more complex structures, use Data.Vector
    If you need to pass to C, use Data.Vector.Storable


Boxed vectors hold normal Haskell values. These can be any values at all, and are stored on the heap with pointers kept in the vector. The advantage is that this works for all datatypes, but the extra memory overhead for the pointers and the indirection of needing to dereference those pointers makes them (relative to the next two types) inefficient.

Storable and unboxed vectors both store their data in a byte array, avoiding pointer indirection. This is more memory efficient and allows better usage of caches. The distinction between storable and unboxed vectors is subtle:

    Storable vectors require data which is an instance of the Storable type class. This data is stored in malloced memory, which is pinned (the garbage collector can't move it around). This can lead to memory fragmentation, but allows the data to be shared over the C FFI.
    Unboxed vectors require data which is an instance of the Prim type class. This data is stored in GC-managed unpinned memory, which helps avoid memory fragmentation. However, this data cannot be shared over the C FFI.

#### Concurrent
forkIO run function in another thread

MVar позволяет делать блокировки
communication beetwen 2 threads using MVar, takeMVar усыпляет тред, если переменная пуста, putMVAr тоже усыпляет, если из неё не считали

Chan позволяет пересылать данные в одну сторону
import Control.Concurrent
import Control.Concurrent.Chan

chanExample = do
  ch <- newChan
  forkIO $ do
    writeChan ch "hello world"
    writeChan ch "now i quit"
  readChan ch >>= print
  readChan ch >>= print

  You have three ways of dealing with this problem (shared memory):

      Locks
      Actors
      Software Transactional Memory

forever     :: (Applicative f) => f a -> f b
{-# INLINE forever #-}
forever a   = let a' = a * > a' in a'


https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell
????
https://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics
????

threadDelay 4

:t replicateM
replicateM :: Applicative m => Int -> m a -> m [a]

Из Chan можно считать переменные только один раз, для этого в каждом треде надо делать dupChan


nonDuplicatedTest = do
    messages <- newChan
    forkIO (messageReader messages "First")
    forkIO (messageReader messages "Second")
    writeChan messages "Hi!"

messageReader channel name = do
    msg <- readChan channel
    putStrLn (name ++ " read: " ++ msg)

#### TypeOperators

#### Traversable

#### Except

#### Cont

#### Lens

https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references
https://mmhaskell.com/blog/2017/6/12/taking-a-close-look-at-lenses

#### Template Haskell

#### Free Monad

https://docviewer.yandex.ru/view/0/?* =p1AR2SQhOovoEvGWdyVBK%2BQ6TBp7InVybCI6InlhLWRpc2stcHVibGljOi8vRWlnU3VxQm1IbkdOZlRKMklrNlNtaC90RUZ1a3d6UDBMTU15ZXZ5dklvWT0iLCJ0aXRsZSI6IjIwMTdfaXRtb19mcDJfbDhfbmEucGRmIiwidWlkIjoiMCIsInl1IjoiODAzODczMTY1MTUwMzY1MDc1NyIsIm5vaWZyYW1lIjpmYWxzZSwidHMiOjE1MjYwNzY5MTE3MDJ9

Даёт монаду бесплатно к имеющемуся функтору. Позволяет делать интерпретатор с помощью композиции интерпретаторов. Тип выражения не превращается во что-то страшное

we can use a bit of Template Haskell to create all the functions for us:

{-# LANGUAGE TemplateHaskell #-}
import Control.Monad.Free.TH

makeFree ''DcNodeOperator

#### Check

#### Coerce



#### GADT

#### TypeFamily

#### ForALL

```haskell
applyTwo :: ([a] -> [a]) -> ([Int], [Bool])             -- doesn't compile
applyTwo f = (f [2, 1, 3], f [True, False])

applyTwo :: forall a . ([a] -> [a]) -> ([Int], [Bool])  -- equivalent form ^
applyTwo f = (f [2, 1, 3], f [True, False])

applyTwo :: (forall a . [a] -> [a]) -> ([Int], [Bool])
applyTwo f = (f [2, 1, 3], f [True, False])

ghci> applyTwo id        -- works like magic
([2,1,3],[True,False])
ghci> applyTwo reverse
([3,1,2],[False,True])

{-# LANGUAGE ExistentialQuantification #-}

data ShowBox = forall a . Show a => SB a  -- existental constructor

showAll :: [ShowBox] -> [String]
showAll = map (\(SB a) -> show a)

ghci> showAll [SB (), SB 1, SB True]  -- again, this magic works
["()","1","True"]

{-# LANGUAGE ScopedTypeVariables #-}

prepend2 :: forall a . a -> [a] -> [a]
prepend2 x xs = pair ++ xs
  where
    pair :: [a]  -- uses same type variable 'a'
    pair = [x, x]

    ghci> :t read
    read :: Read a => String -> a
    ghci> read @Int "3"
    3
    ghci> read @Double "3.0"
    3.0
    ghci> :t read @Int
    read @Int :: String -> Int
```

Usefull Links
--------

[IТМО КТ курс](https://github.com/jagajaga/FP-Course-ITMO)
[ВМ курс](https://math.wtf/course/fp_p2_2017)
[learn yo a frege](https://github.com/y-taka-23/learn-you-a-frege)
[tproger](https://tproger.ru/translations/becoming-productive-in-haskell/)
[write scheme](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
[finger tree](https://en.wikipedia.org/wiki/Finger_tree)
[State monad](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State)
[24 days extension](https://ocharles.org.uk/blog/posts/2014-12-01-24-days-of-ghc-extensions.html)
https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
http://www.cs.nott.ac.uk/~pszgmh/fold.pdf
http://www.cs.nott.ac.uk/~pszgmh/pearl.pdf
http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf
http://dev.stephendiehl.com/hask/#http
http://kseo.github.io/posts/2016-12-10-encodings-of-lense.html
Yesod, Spock, Network, Scoty, Aeson, SitePipe, Wreq, MMorph??
http://www.serpentine.com/wreq/tutorial.html
https://www.google.com/search?q=haskell+reader+monad
