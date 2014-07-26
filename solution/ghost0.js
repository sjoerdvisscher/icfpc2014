var lp = lambdaPos()
var gp = ghostPos(me())

var delta =
  [ lp[0] > gp[0] ? lp[0] - gp[0] : gp[0] - lp[0]
  , lp[1] > gp[1] ? lp[1] - gp[1] : gp[1] - lp[1]
  ]

var hdir = lp[0] > gp[0] ? 1 : 3;
var vdir = lp[1] > gp[1] ? 2 : 0;

var dir = delta[0] > delta[1]
  ? hdir
  : delta[0] < delta[1]
    ? vdir
    : (me() & 1) == 0 ? hdir : vdir;

if (ghostStatus(me()) == 1) {
  dir += dir >= 2 ? -2 : 2;
}

move(dir)
