main=getLine>>=(\n->f>>=(\a->f>>=(\b->print.length.filter(\x->all(\i->a!!i<=x&&x<=b!!i)[0..n-1])$[0..10^3]))).read where f=map read.words<$>getLine
