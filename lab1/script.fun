rec append(xs, ys) =
  if xs = nil then ys else head(xs) : append(tail(xs), ys);;

rec concat(xss) =
  if xss = nil then nil else append(head(xss), concat(tail(xss)));;

val yyy = list(1, list(2, 3), list(4, list(5), 6));;

rec flatten(x) =
  if integer(x) then x : nil
  else if x = nil then x
  else append(flatten(head(x)), flatten(tail(x)));;

rec flatsum(x) =
  if integer(x) then x
  else if x = nil then 0
  else flatsum(head(x)) + flatsum(tail(x));;

rec flatsum_tail(x) =
  let rec loop(stack, s) =
    if stack = nil then s
    else if integer(head(stack)) then loop(tail(stack), s + head(stack))
    else loop(append(head(stack), tail(stack)), s) in
  loop(x, 0);;

rec flatsum_it(x) =
  let val stack = new() in let val s = new() in
  stack := x; s := 0;
  while !stack <> nil do
    (if integer(head(!stack)) then (s := !s + head(!stack); stack := tail(!stack))
     else stack := append(head(!stack), tail(!stack))); -- TODO: use iterative append
  !s;;
