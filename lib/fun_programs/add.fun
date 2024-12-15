
def add(x, y) =
  if x == 0 then y else 1 + (add(x - 1, y));

def addT(x, y) =
  if x == 0 then y else addT(x - 1, y + 1);

(write(add(1000, 1000)); addT(100000,100000))