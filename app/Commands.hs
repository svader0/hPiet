module Commands where

import Eval
import Types

pushBlockValue :: Int -> PietState -> PietState
pushBlockValue value state = state {stack = push value (stack state)}

popValue :: PietState -> PietState
popValue state = let (_, newStack) = pop (stack state) in state {stack = newStack}

add :: PietState -> PietState
add state =
  let (Just val1, s1) = pop (stack state)
      (Just val2, s2) = pop s1
   in state {stack = push (val2 + val1) s2}

subtractOp :: PietState -> PietState
subtractOp state =
  let (Just val1, s1) = pop (stack state)
      (Just val2, s2) = pop s1
   in state {stack = push (val2 - val1) s2}

multiply :: PietState -> PietState
multiply state =
  let (Just val1, s1) = pop (stack state)
      (Just val2, s2) = pop s1
   in state {stack = push (val2 * val1) s2}

divide :: PietState -> PietState
divide state =
  let (Just val1, s1) = pop (stack state)
      (Just val2, s2) = pop s1
   in if val1 == 0 then state else state {stack = push (val2 `div` val1) s2}

modOp :: PietState -> PietState
modOp state =
  let (Just val1, s1) = pop (stack state)
      (Just val2, s2) = pop s1
   in if val1 == 0 then state else state {stack = push (val2 `mod` val1) s2}

notOp :: PietState -> PietState
notOp state =
  let (Just val, s) = pop (stack state)
   in state {stack = push (if val == 0 then 1 else 0) s}

greater :: PietState -> PietState
greater state =
  let (Just val1, s1) = pop (stack state)
      (Just val2, s2) = pop s1
   in state {stack = push (if val2 > val1 then 1 else 0) s2}

pointer :: PietState -> PietState
pointer state =
  let (Just val, s) = pop (stack state)
   in state {dp = (dp state + val) `mod` 4, stack = s} -- Assuming 4 directions

switch :: PietState -> PietState
switch state =
  let (Just val, s) = pop (stack state)
   in state {cc = (cc state + val) `mod` 2, stack = s} -- Assuming toggle

duplicate :: PietState -> PietState
duplicate state =
  let (Just val, s) = pop (stack state)
   in state {stack = push val (push val s)}

roll :: PietState -> PietState
roll state =
  let (Just rolls, s1) = pop (stack state)
      (Just depth, s2) = pop s1
   in if depth < 0 || rolls < 0
        then state
        else
          let (newStack, _) = rollStack (stack state) rolls depth
           in state {stack = newStack}

rollStack :: Stack -> Int -> Int -> (Stack, Bool)
rollStack stack rolls depth
  | depth < 0 || rolls == 0 = (stack, False)
  | depth >= length stack = (stack, True)
  | otherwise =
      let (top, rest) = splitAt depth stack
          (buried, remaining) = splitAt 1 rest
          newStack = buried ++ top ++ remaining
       in rollStack newStack (rolls - 1) depth

inNumber :: PietState -> IO PietState
inNumber state = do
  num <- readLn
  return $ pushBlockValue num state

outNumber :: PietState -> IO ()
outNumber state = do
  let (Just val, _) = pop (stack state)
  print val

outChar :: PietState -> IO ()
outChar state = do
  let (Just val, _) = pop (stack state)
  putChar $ toEnum val