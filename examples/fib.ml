let fib = mu f . fn x .
            if x == 0 then 0 else
            if x == 1 then 1 else
            let px = pred x in
                (f px) + f (pred px)
    in
    fib 5

-- 5