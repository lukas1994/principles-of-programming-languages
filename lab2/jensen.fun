val sum(i, a, b, f) =
  let val s = new() in
  i := a; s := 0;
  while !i < b do (s := !s + f; i := !i + 1);
  !s;;

val go() =
  let val i = new() in
  sum(i, 0, 10, !i * !i);;

go();;

val sum2(i, a, b, f) =
  let val s = new() in
  i := a; s := 0;
  while !i < b do (s := !s + f(); i := !i + 1);
  !s;;

val go2() =
  let val i = new() in
  sum2(i, 0, 10, lambda() !i*!i);;

go2();;

-- call-by-value: only go2 works (executing !i*!i raises an error
--                "uninitialized location 0")
-- call-by-name: both versions work and return 285
