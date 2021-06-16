import bignumber, times

# small digits addition.
var n: BigInt = newBigInt(0)
var t1: float = cpuTime()
for i in 1..10000000:
    n += i
var t2: float = cpuTime()
echo "Sum of 1 to 10000000: " & $(t2 - t1) & " seconds."
echo ""

# small digits multiplication.
n = newBigInt(123456789)
var tmp: BigInt = n
t1 = cpuTime()
for i in 1..10000000:
    tmp = tmp * i
    tmp = n
t2 = cpuTime()
echo "10000000 replicates of small digits multiplication: " & $(t2 - t1) & " seconds."
echo ""

# Five to the ith power.
for i in @[5000, 10000, 50000, 100000, 500000, 1000000]:
    t1 = cpuTime()
    n = newBigInt(5)^i
    t2 = cpuTime()
    echo "Five to the " & $i & "th power: " & $(t2 - t1) & " seconds."
echo ""

# naive factorial with loop.
proc factorial(n: int): BigInt =
    result = newBigInt(1)
    if (n == 0) or (n == 1):
        return
    else:
        for i in 1..n:
            result *= n

for i in @[1000, 5000, 10000, 50000]:
    t1 = cpuTime()
    n = factorial(i)
    t2 = cpuTime()
    echo "Factorial of " & $i & " by loop: " & $(t2 - t1) & " seconds."
echo ""

# factorial with binary splitting.
proc factorialCore(a,b: int): BigInt =
    if b == a + 1:
        result = newBigInt(b)
    else:
        var
            m: int = (a + b) div 2
            am: BigInt = factorialCore(a,m)
            mb: BigInt = factorialCore(m,b)
        result = am * mb

proc factorial2(n: int): BigInt =
    if n == 0:
        result = newBigInt(1)
    else:
        result = factorialCore(0,n)

for i in @[1000, 5000, 10000, 50000, 100000, 500000]:
    t1 = cpuTime()
    n = factorial2(i)
    t2 = cpuTime()
    echo "Factorial of " & $i & " by binary splitting: " & $(t2 - t1) & " seconds."
echo ""

# sqrt(2)
var x: BigFloat
let a: BigFloat = newBigFloat(2)
for i in @[100000, 1000000, 10000000]:
    setPrec(i)
    t1 = cpuTime()
    x = sqrt(a)
    t2 = cpuTime()
    echo "sqrt(2) " & $i & " digits: " & $(t2 - t1) & " seconds."
    
