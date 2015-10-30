val x = new();;
x := 1;;

val p1 = lambda() x := !x+1;;
val p2() = x := !x*10;;

val test0(p) = 2;;
val test1(p) = p+p;2;;

test0(x := !x + 1);;
--test1(x := !x + 1);;

!x;;

-- call-by-value: x will be 2 after running test0 or test1
-- call-by-name: x will be 1 after running test0 (the passed expression doesn't
--               get exectuted)
--               x will be 3 after running test1 (the passed expression gets
--               executed twice)
