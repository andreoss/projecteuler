n = 2 

for i = 1, 7830456 do
   n = ( 2 * n ) % 10000000000;
end

n = (n * 28433 + 1) % 10000000000

print(n)