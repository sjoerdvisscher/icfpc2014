var lp = lambdaPos()
var gp = ghostPos(me())
var delta;
if (lp[0] > gp[0]) {
  delta[0] = lp[0] - gp[0]
} else {
  delta[0] = gp[0] - lp[0]
}
if (lp[1] > gp[1]) {
  delta[1] = lp[1] - gp[1]
} else {
  delta[1] = gp[1] - lp[1]
}
var dir;
if (delta[0] > delta[1]) {
  if (lp[0] > gp[0]) {
    dir = 1 // right
  } else {
    dir = 3 // down
  }
} else {
  // move vertically
  if (lp[1] > gp[1]) {
    dir = 2 // down
  } else {
    dir = 0 // up
  }
}
if (ghostStatus(me()) == 1) {
  if (dir >= 2)
    dir -= 2;
  else
    dir += 2;
}
move(dir)
