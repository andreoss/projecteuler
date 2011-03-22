import qualified Data.Set as S

answer = S.size $ S.fromList [ a^b | a<-[2..100],  b<-[2..100]]

main   = do print answer
