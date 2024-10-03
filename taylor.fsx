let eps = 1e-7

let builtin x = 
  let chisl = 2. * x - 3.
  let znam = (x - 1.) ** 2.
  let res = chisl / znam
  res

let get_taylor_naive_part x n = 
  let res = - (n + 3.) * x ** n
  res

let taylor_naive x = 
  let mutable sum = 0.0
  let mutable curr_elem = get_taylor_naive_part x 0
  let mutable iters = 0
  while abs(curr_elem) > eps do
      sum <- sum + curr_elem
      iters <- iters + 1
      curr_elem <- get_taylor_naive_part x iters
  (sum, iters)

let get_taylor_smart_part x n = 
  let chisl = (n + 3.) * x
  let znam = n + 2.
  let res = chisl / znam
  res

let taylor_smart x = 
  let mutable sum = 0.0
  let mutable curr_elem = -3.
  let mutable iters = 0
  while abs(curr_elem) > eps do
      sum <- sum + curr_elem
      iters <- iters + 1
      curr_elem <- curr_elem * get_taylor_smart_part x iters
  (sum, iters)

let main =
    let a = 0.1
    let b = 0.6
    let n = 10
    printfn "---------------------------------------------------------------------------------"
    printfn "|  x  |    Builtin   |  Smart Taylor|    #terms     |   Dumb Taylor|     #terms |"
    printfn "---------------------------------------------------------------------------------"
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        printfn "|%5.2f|  %10.6f  |  %10.6f  |   %10d  |  %10.6f  |    %10d  |" 
            x (builtin x) (fst (taylor_naive x)) (snd (taylor_naive x )) (fst (taylor_smart x)) (snd (taylor_smart x)) 
    printfn "----------------------------------------------------------------------------"

main