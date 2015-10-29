val sum(i, a, b, f) =
  let val s = new() in
  i := a; s := 0;
  while !i < b do (s := !s + f; i := !i + 1);
  !s;;

val go() =
  let val i = new() in
  sum(i, 0, 10, !i * !i);;

go();;

val sum2(a, b, f) =
  let val s = new() in
  let val i = new() in
  i := a; s := 0;
  while !i < b do (s := !s + f(!i); i := !i + 1);
  !s;;

val go2() =
  sum2(0, 10, lambda(x) x*x);;

go2();;
