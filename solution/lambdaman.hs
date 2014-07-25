main world0 unk = (map (\row -> map (\v -> if v then 1 else 0) row) (fst world0), step)

step moveCounts w =
  let wmap = fst w in
  let lambda = fst (snd w) in
  let lpos = pos lambda in
  let ghosts = fst (snd (snd w)) in
  let poss = filter (\p -> read wmap p) (map (\d -> move lpos d) [0,1,2,3]) in
  let measureDist = (\p -> minBy id (map (\g -> len (sub p (pos g))) ghosts)) in
  let measureCount = (\p -> read moveCounts p) in
  let bestPos = if vit lambda then minBy measureDist poss else minBy measureCount poss in
  let bestDir = dir (sub bestPos lpos) in
  (update moveCounts lpos (\c -> c + 1), bestDir)

l !! n = if n then snd l !! (n - 1) else fst l
foldr f z l = if atom l then z else f (fst l) (foldr f z (snd l))
map f l = foldr (\a t -> f a : t) [] l
filter f l = foldr (\a t -> if f a then a : t else t) [] l
sum l = foldr (\x y -> x + y) 0 l
minBy f l = snd (foldr (\a b -> let d = f a in if d < fst b then (d, a) else b) (100000, 0) l)
maxBy f l = snd (foldr (\a b -> let d = f a in if d > fst b then (d, a) else b) (-100000, 0) l)
abs x = if x > 0 then x else -1 * x
add p q = (fst p + fst q, snd p + snd q)
sub p q = (fst p - fst q, snd p - snd q)
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
inv x = let d = x + 2 in if d > 3 then d - 4 else d

id x = x
a < b = b > a
a != b = if a == b then 0 else 1
