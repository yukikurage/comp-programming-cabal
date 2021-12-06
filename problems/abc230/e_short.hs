main=interact$show.(\n->sum(map(\x->g$n/f x)[1..g$sqrt n])+sum(map(\x->x*(g(n/f x)-g(n/(f x+1))))[1..g(n/f(g$sqrt n))-1])).read where f=fromIntegral;g=floor
