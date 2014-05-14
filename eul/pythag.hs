pythag :: Int -> [(Int,Int,Int)]
pythag n = [(p^2+q^2,2*p*q,p^2-q^2)|p<-[1..],q<-[1..p],p>q,2*p^2+2*p*q<=n]
