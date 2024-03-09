module Stack where

import Types

-- Basic stack operations
push :: Int -> Stack -> Stack
push = (:)

pop :: Stack -> (Maybe Int, Stack)
pop [] = (Nothing, [])
pop (x : xs) = (Just x, xs)

add :: Stack -> (Maybe Int, Stack)
add [] = (Nothing, [])
add [x] = (Nothing, [x])
add (x : y : xs) = (Just (x + y), xs)

subtract :: Stack -> (Maybe Int, Stack)
subtract [] = (Nothing, [])
subtract [x] = (Nothing, [x])
subtract (x : y : xs) = (Just (y - x), xs)

multiply :: Stack -> (Maybe Int, Stack)
multiply [] = (Nothing, [])
multiply [x] = (Nothing, [x])
multiply (x : y : xs) = (Just (x * y), xs)

divide :: Stack -> (Maybe Int, Stack)
divide [] = (Nothing, [])
divide [x] = (Nothing, [x])
divide (x : y : xs) = (Just (y `div` x), xs)

modulo :: Stack -> (Maybe Int, Stack)
modulo [] = (Nothing, [])
modulo [x] = (Nothing, [x])
modulo (x : y : xs) = (Just (y `mod` x), xs)
