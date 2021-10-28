# Calculating Pi with Chudnovsky series. 
# Reference: Fabrice Bellard (2010) Computation of 2700 billion decimal digits of Pi using a Desktop Computer.
# For optimal performance, build with 'nim compile -d:release --checks:off --gc:arc --cc:clang --passC:"-march=native" chudnovskyPi.nim'.
# To calculate 1000000 digits of Pi, run "./chudnovskyPi 1000000".

import bignumber, math, os, strutils, times

type
    PQT = ref object of RootObj
        p: BigFloat
        q: BigFloat
        t: BigFloat

const
    A: int64 = 13591409
    B: int64 = 545140134

let C: BigFloat = newBigFloat("10939058860032000")

proc computePQT(a: int64, b: int64): PQT =
    if b == a + 1:
        result = new PQT
        result.p = newBigFloat(1 - 2 * b) * newBigFloat(6 * b - 5) * newBigFloat(6 * b - 1)
        result.q = newBigFloat(b) * newBigFloat(b * b) * C
        result.t = result.p * newBigFloat(A + B * b)
    else:
        var
            m = (a + b) div 2
            am = computePQT(a, m)
            mb = computePQT(m, b)
        result = new PQT
        result.p = am.p * mb.p
        result.q = am.q * mb.q
        result.t = am.t * mb.q + am.p * mb.t

proc calcPi(): BigFloat =
    var 
        terms: int64 = (int64(getPrec()).toBiggestFloat / 14.181647462).ceil.toBiggestInt + 10
        pqt: PQT = computePQT(0, terms)
        num: BigFloat
        den: BigFloat
    num = pqt.q * newBigFloat("426880") * newBigFloat("10005").sqrt()
    den = pqt.t + (pqt.q * newBigFloat("13591409"))
    result = num / den

let digits: int = paramStr(1).parseInt()
setPrec(digits+32)
echo "This program might take much time for large digits. Press Ctrl + C to quit."
var fileName: string = "pi_"
fileName.add($digits)
fileName.add("digits.txt")
var f: File = open(fileName, FileMode.fmWrite)
let t1: float = cpuTime()
f.writeLine ($calcPi())[0..digits+1]
let t2: float = cpuTime()
echo "Finished! Result is written in " & fileName & "."
echo "Elapsed time is " & $(t2 - t1) & " seconds."

