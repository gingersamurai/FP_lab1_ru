// Определите функции для решение алгебраических уравнений

let eps = 1e-4
let sgn a = if a > 0. then 1. else if a < 0. then -1. else 0.

let rec dichotomy f a b =
    if sgn(f a) = sgn(f b) then
        b
    else
        let c = (a + b) / 2. 
        if f c = 0. || (c - a) < eps then
            c
        else if ((f a) * (f c) < 0.) then
            dichotomy f a c
        else
            dichotomy f c b

let rec iterations phi x0 = 
    if abs (x0 - (phi x0)) < eps then
        x0
    else
        let next = phi x0
        iterations phi next
let newthon f f' x0 = 
    let phi x : float = x - (f x) / (f' x)
    iterations phi x0

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = tan (x / 2.) - (1. / tan (x / 2.)) + x
let f2 x = 0.4 + atan(sqrt(x)) - x 
let f3 x = 3. * sin(sqrt(x))  + 0.35 * x - 3.8

let f1' x = 1. /  (2. *  cos ((x / 2.) ** 2.)) + 1. /  (2. *  sin ((x / 2.) ** 2)) + 1.
let f2' x = 1. / (2. * sqrt(x) + 2. * x * sqrt(x)) - 1.
let f3' x = (3. * cos(sqrt(x))) / (2. * sqrt(x)) + 0.35

let phi1 x = - tan (x / 2.) + (1. / tan (x / 2.))
let phi2 x = 0.4 + atan(sqrt(x))
let phi3 x = (-3. * sin(sqrt(x)) + 3.8) / 0.35

let main = 

    printfn "---------------------------------------------------------------------------"
    printfn "|    f    |  a  |  b  |    Diho    |    Iter    |   Nueton   |  Apr  Val  |"
    printfn "---------------------------------------------------------------------------"
    printfn "|    1    |  1  |  2  |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (dichotomy f1 1. 2.) (iterations phi1 1.5) (newthon f1 f1' 1.5) 1.0769
    printfn "---------------------------------------------------------------------------" 
    printfn "|    1    |  1  |  2  |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (dichotomy f2 1. 2.) (iterations phi2 1.5) (newthon f2 f2' 1.5) 1.2388
    printfn "---------------------------------------------------------------------------" 
    printfn "|    1    |  1  |  2  |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (dichotomy f3 1. 2.) (iterations phi3 2.5) (newthon f3 f3' 2.5) 2.2985
    printfn "---------------------------------------------------------------------------" 


main