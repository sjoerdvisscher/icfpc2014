main world0 unk = ((0, map (\row -> map (\v -> if v then 1 else 0) row) (fst world0)), step)

step state w =
  let moveCounts = snd state in
  let lastDirInv = inv (fst state) in
  let wmap = fst w in
  let lambda = fst (snd w) in
  let lpos = pos lambda in
  let ghosts = fst (snd (snd w)) in
  let allDirs = [0,1,2,3] in
  let idealDirs = filter (\d -> d != lastDirInv) allDirs in
  let possible = filter (\p -> read wmap (snd p)) (map (\d -> (d, move lpos d)) [0,1,2,3]) in
  let allPos = map (\p -> snd p) possible in
  let idealPos = if length allPos == 1 then allPos else map (\p -> snd p) (filter (\p -> fst p != lastDirInv) possible) in
  let measureDist = (\p -> minBy id (map (\g -> len (sub p (pos g))) ghosts)) in
  let measureCount = (\p -> read moveCounts p) in
  let bestPos = (if vit lambda then minBy measureDist idealPos else if measureDist lpos < 5 then maxBy measureDist allPos else minBy measureCount idealPos) in
  let bestDir = dir (sub bestPos lpos) in
  ((bestDir, update moveCounts lpos (\c -> c + 1)), bestDir)

l !! n = if n then snd l !! (n - 1) else fst l
foldr f z l = if atom l then z else f (fst l) (foldr f z (snd l))
length l = foldr (\_ t -> 1 + t) 0 l
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
