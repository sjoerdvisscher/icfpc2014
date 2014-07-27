main world0 unk = ((0, mkStepsMap (fst world0), [0,1,2,3], mkFruitMap world0), step)

step state w =
  let lastDirInv = inv (fst state) in
  let moveCounts = fst (snd state) in
  let directions = fst (snd (snd state)) in
  let fruitMap = snd (snd (snd state)) in
  let wmap = fst w in
  let lambda = fst (snd w) in
  let lpos = pos lambda in
  let ghosts = fst (snd (snd w)) in
  let isGhost = (\p -> sum (map (\g -> eq p (pos g)) ghosts)) in
  let getPossible = (\p -> filter (\p' -> read wmap p') (map (\d -> move p d) directions)) in
  let measureDist = (\p -> findGhost isGhost getPossible p (-1,-1) 5) in
  let dists = map (\p -> (p, measureDist p)) (getPossible lpos) in
  let closest = fst (minBy (\p -> snd p) dists) in
  let ghostDist = snd closest in
  let possible = filter (\p -> read moveCounts (snd p) * ((ghostDist < 3) + (read wmap (snd p) != 3))) (map (\d -> (d, move lpos d)) directions) in
  let measureCount = (\p -> read moveCounts (snd p) + if fst p == lastDirInv then 4 else 0) in
  let bestPos = (if vit lambda then fst closest else if ghostDist < 4 then fst (fst (maxBy (\p -> snd p) dists)) else snd (fst (minBy measureCount possible))) in
  let bestDir = dir (sub bestPos lpos) in
  ((bestDir, update moveCounts lpos (\c -> if length possible == 1 then 0 else if c == 1 then 3 else c + 1), snd directions ++ [fst directions], fruitMap), bestDir)

findGhost isGhost getPossible p from steps =
  if steps
    then if isGhost p
      then 0
      else 1 + fst (minBy id (map (\p' -> findGhost isGhost getPossible p' p (steps - 1)) (filter (\p' -> neq p' from) (getPossible p))))
    else 100

mkStepsMap w =
  (from (length w) (\y s ->
    from (length (fst w)) (\x s ->
      let p = (x, y) in
      let v = read s p in
      if v then if (v == 1) then trackDeadEnd s p else update s p (\_ -> 1) else s
    ) s
  ) w)
trackDeadEnd s p =
  let nonwalls = filter (\p -> read s p) (map (\d -> move p d) [0,1,2,3]) in
  if length nonwalls == 1 then trackDeadEnd (update s p (\_ -> 0)) (fst nonwalls) else update s p (\_ -> 2)
mkFruitMap w = 0

l !! n = if n then snd l !! (n - 1) else fst l
foldr f z l = if atom l then z else f (fst l) (foldr f z (snd l))
l ++ r = foldr (\a b -> a : b) r l
length l = foldr (\_ t -> 1 + t) 0 l
map f l = foldr (\a t -> f a : t) [] l
filter f l = foldr (\a t -> if f a then a : t else t) [] l
sum l = foldr (\x y -> x + y) 0 l
minBy f l = map (\x -> snd x) (foldr (\a b -> let d = f a in let d' = fst (fst b) in if d < d' then [(d, a)] else if d == d' then (d, a):b else b) [(100000, 0)] l)
maxBy f l = map (\x -> snd x) (foldr (\a b -> let d = f a in let d' = fst (fst b) in if d > d' then [(d, a)] else if d == d' then (d, a):b else b) [(-100000, 0)] l)
abs x = if x > 0 then x else -1 * x
add p q = (fst p + fst q, snd p + snd q)
sub p q = (fst p - fst q, snd p - snd q)
eq p q = (fst p == fst q) * (snd p == snd q)
neq p q = not (eq p q)
len p = abs (fst p) + abs (snd p)
dir p = let x = fst p in let y = snd p in
  if abs x > abs y then
    if x > 0 then 1 else 3
  else
    if y > 0 then 2 else 0
move p d = if d == 0 then (fst p, snd p - 1) else
  if d == 1 then (fst p + 1, snd p) else
  if d == 2 then (fst p, snd p + 1) else (fst p - 1, snd p)
read m p = m !! snd p !! fst p
update m p f = modify m (snd p) (\row -> modify row (fst p) f)
modify l n f = if n then fst l : modify (snd l) (n - 1) f else f (fst l) : snd l
vit x = car x
pos x = car (cdr x)
dirl x = car (cdr (cdr x))
dirg x = cdr (cdr x)
inv x = if x >= 2 then x - 2 else x + 2
from n f s = if n then from (n - 1) f (f (n - 1) s) else s

id x = x
a < b = b > a
a != b = not (a == b)
not b = if b then 0 else 1
