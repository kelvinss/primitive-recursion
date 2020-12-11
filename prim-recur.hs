import Prelude(Int, (+), (-), (.))
type N = Int
type Func x = x -> N -> N

wrapx f x = f
unwrapx f = f ()

wrapm f x m = f x

-- basic operations

zero _ = 0
succ n = n + 1

zerox = wrapx zero
succx = wrapx succ

--       f1          f2                    x    mm
recur :: (x -> N) -> (x -> N -> N -> N) -> x -> N -> N

recur f1 f2 x 0  = f1 x

recur f1 f2 x mm = f2 x m (h x m)
    where
        m = mm - 1
        h = recur f1 f2

-- --

id = unwrapx idx
idx =
    recur
        zero
        (\x m n -> succ n)

one = unwrapx idx
onex =
    recur
        one
        (\x m n -> succ n)

add =
    recur
        id
        (\x m n -> succ n)

pred = unwrapx predx
predx =
    recur
        zero
        (\x m n -> m)

sub =
    recur
        id
        (\x m n -> pred n)

not = unwrapx notx
notx =
    recur
        (succ . zero)
        (\x m n -> zero x)

mod2 = unwrapx mod2x
mod2x =
    recur
        zero
        (\x m n -> not n)
