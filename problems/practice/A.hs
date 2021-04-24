main=getContents>>=(\x->putStr$(show.sum.map read.take 3$x)++" "++last x).words
