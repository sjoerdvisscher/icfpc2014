var history = new Array(24);
var historyPos;
var lp = lambdaPos();
var gp = ghostPos(me());
var mode = ghostStatus(me());

var delta =
  [ lp[0] > gp[0] ? lp[0] - gp[0] : gp[0] - lp[0]
  , lp[1] > gp[1] ? lp[1] - gp[1] : gp[1] - lp[1]
  ];

var dist = delta[0] + delta[1];

if (dist > 20) {
  lp = ghostPos(me() ^ 1);
  delta[0] = lp[0] > gp[0] ? lp[0] - gp[0] : gp[0] - lp[0];
  delta[1] = lp[1] > gp[1] ? lp[1] - gp[1] : gp[1] - lp[1];
}

var hdir = lp[0] > gp[0] ? 1 : 3;
var vdir = lp[1] > gp[1] ? 2 : 0;

var dir = delta[0] > delta[1]
  ? hdir
  : delta[0] < delta[1]
    ? vdir
    : (me() & 1) == 0 ? hdir : vdir;

if (mode == 1) {
  dir += dir >= 2 ? -2 : 2;
} else {
  if (dist > 20) {
    dir += dir >= 2 ? -2 : 2;
  }
  for (var i = 0; i < 24; i += 3) {
    if (history[i] == gp[0]) {
      if (history[i + 1] == gp[1]) {
        dir = history[i + 2] + 1;
        if (dir == 4) {
          dir = 0;
        }
      }
    }
  }
}

move(dir);

history[historyPos] = gp[0];
history[historyPos + 1] = gp[1];
history[historyPos + 2] = dir;
historyPos += 3;
if (historyPos == 24)
  historyPos = 0;
