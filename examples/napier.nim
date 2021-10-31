# Calculating Napier's constant e.
# For optimal performance, build with 'nim compile -d:release --checks:off --gc:arc --cc:clang --passC:"-march=native" napier.nim'.
# To calculate 1000000 digits of e, run "./napier 1000000".
import bignumber, math, os, strutils, times

type
    PQ = ref object of RootObj
        p: BigInt
        q: BigInt
        
proc computePQ(a, b: int): PQ =
    if b == a + 1:
        result = new PQ
        result.p = newBigInt(1)
        result.q = newBigInt(b)
    else:
        let 
            m: int = (a + b) div 2
            am: PQ = computePQ(a, m)
            mb: PQ = computePQ(m, b)
        result = new PQ
        result.p = am.p * mb.q + mb.p
        result.q = am.q * mb.q

proc requiredTerms(prec: int): int = 
    var 
        precFloat: float = prec.toFloat
        t: float = 0.0
    for i in 1..prec:
        t += log10(i.toFloat)
        if t > precFloat + 10.0:
            result = i
            break

proc calcNapier(): BigFloat =
    var
        terms: int = requiredTerms(getPrec())
        pq: PQ = computePQ(0, terms)
        num: BigFloat = newBigFloat((pq.p + pq.q))
        den: BigFloat = newBigFloat(pq.q)
    result = num / den

let digits: int = paramStr(1).parseInt()
setPrec(digits+32)
echo "This program might take much time for large digits. Press ctrl + C to quit."
var fileName: string = "e_"
fileName.add($digits)
fileName.add("digits.txt")
var f: File = open(fileName, FileMode.fmWrite)
let t1: float = cpuTime()
f.writeLine ($calcNapier())[0..digits+1]
let t2: float = cpuTime()
echo "Finished! Result is written in " & fileName & "."
echo "Elapsed time is " & $(t2 - t1) & " seconds."
