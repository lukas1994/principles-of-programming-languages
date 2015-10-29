val x = new();;
x := 1;;

val p1() = x := !x+1;;
val p2() = x := !x*10;;

val test(p1, p2) = p1+p1;;

test(p1(), p2());;
