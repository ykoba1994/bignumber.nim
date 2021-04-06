## Arbitrary precision integers and floating point numbers for Nim.

import strutils, algorithm, sequtils, math

type
    BigInt* = ref object of RootObj
        sign*: bool  # true for 0 or positive values, false for negative values.
        limbs*: seq[int64]
        
    BigFloat* = ref object of RootObj
        intPart*: BigInt
        exp*: int
        
    BigFloatContext = ref object of RootObj
        prec: int
 
const 
    BASE: int64 = 10000000000000000
    BASE2: int64 = 100000000
    LOG_BASE: int = 16
    KARATSUBA_THRESHOLD: int = 50
    TOOM3_THRESHOLD: int = 250
    TOOM4_THRESHOLD: int = 900
    validCharsForBigInt: string = "01234567890"
    validCharsForBigFloat: string = ".0123456789"
    
let    
    zero*: BigInt = BigInt(sign: true, limbs: @[0'i64])

var
    bfContext: BigFloatContext = new BigFloatContext

proc newBigIntNoCheck(s: string): BigInt =
    var
        s2: string
        inputLength: int
        limbsLength: int
        reminderLength: int
    result = new BigInt
    s2 = s[0..^1]   
    if $s2[0] == "-":
        result.sign = false
        s2.delete(0, 0)
    elif $s2[0] == "+":
        result.sign = true
        s2.delete(0, 0)  
    else:
        result.sign = true
    result.limbs = @[]
    inputLength = len(s2)
    limbsLength = inputLength div LOG_BASE
    reminderLength = inputLength mod LOG_BASE
    if reminderLength != 0:
        result.limbs.add(parseBiggestInt(s2[0..reminderLength - 1]))
        s2.delete(0,reminderLength - 1)
    for i in 0..(limbsLength - 1):
        result.limbs.add(parseBiggestInt(s2[LOG_BASE*i..(LOG_BASE*i + LOG_BASE-1)]))
    result.limbs.reverse
    # The sign of zero is always positive.
    if result.limbs == @[0'i64]:
        result.sign = true

proc newBigInt*(s: string, checkInput: bool = true): BigInt =
    if checkInput:
        var s3: string = s[0..^1]
        if ($s3[0] == "+") or ($s3[0] == "-"):
            s3.delete(0,0)
        for i in (0..<len(s3)):
            if $s3[i] == "+":
                raise newException(ValueError, "Not leading '+' in input string.")
            elif $s3[i] == "-":
                raise newException(ValueError, "Not leading '-' in input string.")
            elif not ($s3[i] in validCharsForBigInt):
                raise newException(ValueError, "Invalid character(s) in input string.")
    result = newBigIntNoCheck(s)

proc newBigInt*(a: int64): BigInt =
    var
        n: int64
    result = new BigInt
    if a < 0:
        result.sign = false
        result.limbs.add(a.abs())
        n = a.abs()
    else:
        result.sign = true
        result.limbs.add(a)
        n = a
    if n > BASE:
        result.limbs[0] = n mod BASE
        result.limbs.add(n div BASE)
    
proc newBigInt*[T: int8|uint8|int16|uint16|int32|uint32|int](a: T): BigInt =
    result = newBigInt(int64(a))

proc newBigInt*(a: uint64): BigInt =
    result = newBigIntNoCheck($a)

proc toStr*(x: BigInt): string = 
    var
        x2: seq[int64]
        s: string
        t: string
        t2: string
        n: int
    x2 = x.limbs[0..^1]
    x2.reverse
    s = $(x2[0])
    for i in 1..(len(x2) - 1):
        t = $(x2[i])
        n = len(t)
        t2 = "0".repeat(LOG_BASE - n)
        t2.add(t)
        s.add(t2)
    if x.sign == false:
        result = "-"
        result.add(s)
    else:
        result = s

proc `$` *(x: BigInt): string =
    result = x.toStr()

proc cmp(x, y: BigInt): int =
    var
        count: int = 0
        m: int = len(x.limbs)
        n: int = len(y.limbs)
    if x.sign and (not y.sign):
        return 1
    elif (not x.sign) and y.sign:
        return -1
    elif x.sign and y.sign:
        if m > n:
            return 1
        elif m < n:
            return -1
        else:
            if x.limbs == y.limbs:
                return 0
            else:
                for i in countdown((m - 1),0):
                    if x.limbs[i] > y.limbs[i]:
                        count += 1
                        break
                    elif x.limbs[i] < y.limbs[i]:
                        count -= 1
                        break
                if count > 0:
                    return 1
                else:
                    return -1
    else:
        if m > n:
            return -1
        elif m < n:
            return 1
        elif x.limbs == y.limbs:
                return 0
        else:
            for i in countdown((m - 1),0):
                if x.limbs[i] > y.limbs[i]:
                    count += 1
                    break
                elif x.limbs[i] < y.limbs[i]:
                    count -= 1
                    break
            if count > 0:
                return -1
            else:
                return 1

proc `>` *(x, y: BigInt): bool =
    return x.cmp(y) > 0

proc `>=` *(x, y: BigInt): bool =
    return x.cmp(y) >= 0

proc `==` *(x, y: BigInt): bool =
    return x.cmp(y) == 0

proc `<=` *(x, y: BigInt): bool =
    return x.cmp(y) <= 0

proc `<` *(x, y: BigInt): bool =
    return x.cmp(y) < 0

proc `!=` *(x, y: BigInt): bool =
    return x.cmp(y) != 0

proc `-` *(x: BigInt): BigInt =
    if x == zero:
        result = zero
    else:
        result = new BigInt
        result.sign = not x.sign
        result.limbs = x.limbs
    
proc abs*(x: BigInt): BigInt =
    result = new BigInt
    result.sign = true
    result.limbs = x.limbs

proc max*(x, y: BigInt): BigInt =
    if x >= y:
        result = x
    else:
        result = y

proc min*(x, y: BigInt): BigInt =
    if x <= y:
        result = x
    else:
        result = y

proc removeLeadingZeros(x: var BigInt) =
    var n: int
    while (x.limbs[^1] == 0'i64) and (len(x.limbs) > 1):
        n = len(x.limbs) - 1
        x.limbs.delete(n, n)

# Unsigned addition. Only works when x >= y >= 0.
proc uadd(x, y: BigInt): BigInt =
    var 
        m: int = len(x.limbs)
        n: int = len(y.limbs)
    result = new BigInt
    result.limbs = newSeq[int64](m + 1)
    result.sign = true
    for i in 0..(m - 1):
        result.limbs[i] = x.limbs[i]
    for i in 0..(n - 1):
        result.limbs[i] += y.limbs[i]
        if result.limbs[i] >= BASE:
            result.limbs[i] -= BASE 
            result.limbs[i+1] += 1 
    for i in n..m:
        if result.limbs[i] >= BASE:
            result.limbs[i] -= BASE 
            result.limbs[i+1] += 1 
        else:
            break
    result.removeLeadingZeros()

# Unsigned subtraction. Only works when x >= y >= 0.
proc usub(x, y: BigInt): BigInt =
    var 
        m: int = len(x.limbs)
        n: int = len(y.limbs)
    result = new BigInt    
    result.limbs = x.limbs[0..^1] 
    result.sign = true
    for i in 0..(n - 1):
        result.limbs[i] -= y.limbs[i]
        if result.limbs[i] < 0'i64:
            result.limbs[i] += BASE
            result.limbs[i+1] -= 1    
    for i in n..(m - 1):
        if result.limbs[i] < 0'i64:
            result.limbs[i] += BASE
            result.limbs[i+1] -= 1
        else:
            break    
    result.removeLeadingZeros()

proc `+` *(x, y: BigInt): BigInt =
    if abs(x) < abs(y):
        result = y + x
    else:
        if (x >= zero) and (y >= zero):
            result = x.uadd(y)
        elif (x < zero) and (y < zero):
            result = -(abs(x).uadd(abs(y)))
        elif (x >= zero) and (y < zero):
            result = x.usub(abs(y))
        else:
            result = -(abs(x).usub(y))
            # The sign of zero is always positive.
            if result.limbs == @[0'i64]:
                result.sign = true

proc `+` *(x: BigInt, y: SomeInteger): BigInt =
    result = x + newBigInt(y)

proc `+` *(x: SomeInteger, y: BigInt): BigInt =
    result = newBigInt(x) + y

proc `-` *(x, y: BigInt): BigInt =
    result = x + (-y)

proc `-` *(x: BigInt, y: SomeInteger): BigInt =
    result = x - newBigInt(y)

proc `-` *(x: SomeInteger, y: BigInt): BigInt =
    result = newBigInt(x) - y

# Schoolbook multiplication. Time complexity is O(n^2).
proc schoolbookMul(x, y: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: seq[int64] = newSeqUninitialized[int64](2 * m)
        yLimbsSplitted: seq[int64] = newSeqUninitialized[int64](2 * n)
        resultLimbsSplitted: seq[int64]
    if (x == zero) or (y == zero):
        result = zero
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
        for i in 0..(n-1):
            t = y.limbs[i] div BASE2
            yLimbsSplitted[2*i] = y.limbs[i] - t*BASE2
            yLimbsSplitted[2*i + 1] = t
        resultLimbsSplitted = newSeq[int64](2*m + 2*n + 2)
        for i in 0..(2*m - 1):
            xi = xLimbsSplitted[i]
            for k in 0..(2*n - 1):
                resultLimbsSplitted[i+k] += xi * yLimbsSplitted[k]
        result = new BigInt
        result.sign = (x.sign == y.sign)
        for i in 0..(2*m + 2*n + 1):
            t = resultLimbsSplitted[i]
            if t >= BASE2:
                t2 = t div BASE2
                resultLimbsSplitted[i] -= t2 * BASE2
                resultLimbsSplitted[i+1] += t2
        result.limbs = newSeq[int64](len(resultLimbsSplitted) div 2)
        for i in 0..((len(resultLimbsSplitted) div 2) - 1):
            result.limbs[i] = resultLimbsSplitted[2 * i] + (resultLimbsSplitted[2 * i + 1] * BASE2)
        result.removeLeadingZeros()

# Squaring is implemented separately from multiplication for better performance.
proc schoolbookSqr(x: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: seq[int64] = newSeqUninitialized[int64](2 * m)
        resultLimbsSplitted: seq[int64]
    if x == zero:
        result = zero
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
        resultLimbsSplitted = newSeq[int64](4*m + 2)
        for i in 0..(2*m - 1):
            xi = xLimbsSplitted[i]
            for k in (i + 1)..(2*m - 1):
                resultLimbsSplitted[i+k] += xi * xLimbsSplitted[k] * 2
        for i in 0..(2*m - 1):
            resultLimbsSplitted[2*i] += xLimbsSplitted[i]^2
        result = new BigInt
        result.sign = true
        for i in 0..(4*m + 1):
            t = resultLimbsSplitted[i]
            if t >= BASE2:
                t2 = t div BASE2
                resultLimbsSplitted[i] -= t2 * BASE2
                resultLimbsSplitted[i+1] += t2
        result.limbs = newSeq[int64](len(resultLimbsSplitted) div 2)
        for i in 0..((len(resultLimbsSplitted) div 2) - 1):
            result.limbs[i] = resultLimbsSplitted[2*i] + (resultLimbsSplitted[2*i + 1] * BASE2)
        result.removeLeadingZeros()

# Karatsuba multiplication. Time complexity is O(n^1.585).
proc karatsubaMul(x, y: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        a: int = min(m, n) div 2
    if (m < KARATSUBA_THRESHOLD) or (n < KARATSUBA_THRESHOLD):
        result = x.schoolbookMul(y)
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(m - 1)])
            y0: BigInt = BigInt(sign: true, limbs: y.limbs[0..(a - 1)])
            y1: BigInt = BigInt(sign: true, limbs: y.limbs[a..(n - 1)])
            z2: BigInt
            z1: BigInt
            z0: BigInt         
        x0.removeLeadingZeros()
        y0.removeLeadingZeros()
        z2 = x1.karatsubaMul(y1)
        z0 = x0.karatsubaMul(y0)
        z1 = (z2 + z0) - ((x1 - x0).karatsubaMul(y1 - y0))
        z2.limbs.insert(newSeq[int64](2 * a), 0)
        if z1.limbs != @[0'i64]:
            z1.limbs.insert(newSeq[int64](a), 0)
        result = z2 + z1 + z0
        result.sign = (x.sign == y.sign)

# Karatsuba squaring.
proc karatsubaSqr(x: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        a: int = m div 2
    if m < KARATSUBA_THRESHOLD:
        result = x.schoolbookSqr()
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(m - 1)])
            z2: BigInt
            z1: BigInt
            z0: BigInt
        x0.removeLeadingZeros()
        z2 = x1.karatsubaSqr()
        z0 = x0.karatsubaSqr()
        z1 = (z2 + z0) - (x1 - x0).karatsubaSqr()
        z2.limbs.insert(newSeq[int64](2 * a), 0)
        if z1.limbs != @[0'i64]:
            z1.limbs.insert(newSeq[int64](a), 0)
        result = z2 + z1 + z0
        result.sign = true

# Multiplication by small integers. Only used for Toom Cook multiplication.        
proc mulInt(x: BigInt, y: int64): BigInt =
    var 
        m: int = len(x.limbs)
        t: int64
        t2: int64
        #y2: int64 = y    
    result = new BigInt
    result.sign = x.sign
    #if y2 < 0:
        #result.sign = not x.sign
        #y2 = -y2
    result.limbs = newSeqUninitialized[int64](m+2)
    for i in 0..(m-1):
        result.limbs[i] = x.limbs[i] * y
    result.limbs[m] = 0'i64
    result.limbs[m+1] = 0'i64
    for i in 0..(m+1):
        t = result.limbs[i]
        if t >= BASE:
            t2 = t div BASE
            result.limbs[i] -= t2 * BASE
            result.limbs[i+1] += t2
    result.removeLeadingZeros()
    if result.limbs == @[0'i64]:
        result.sign = true

# Division by small integers. Only used for Toom Cook multiplication. 
proc divInt(x: BigInt, y: int64): BigInt = 
    var t: int64
    result = new BigInt
    result.limbs = x.limbs[0..^1]
    result.sign = x.sign
    for i in countdown((len(x.limbs) - 1), 1):
        t = result.limbs[i]
        result.limbs[i] = t div y
        result.limbs[i-1] += (t - (result.limbs[i] * y)) * BASE
    result.limbs[0] = result.limbs[0] div y
    result.removeLeadingZeros()
    if result.limbs == @[0'i64]:
        result.sign = true

# Toom Cook 3 multiplication. Time complexity is O(n^1.465).
# Evaluation points are infinity, 1, -1, -2 and 0.
proc toom3Mul(x, y: BigInt): BigInt = 
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        a: int = min(m, n) div 3
    if (m < TOOM3_THRESHOLD) or (n < TOOM3_THRESHOLD):
        result = x.karatsubaMul(y)
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(2 * a - 1)])
            x2: BigInt = BigInt(sign: true, limbs: x.limbs[(2 * a)..(m - 1)])
            y0: BigInt = BigInt(sign: true, limbs: y.limbs[0..(a - 1)])
            y1: BigInt = BigInt(sign: true, limbs: y.limbs[a..(2 * a - 1)])
            y2: BigInt = BigInt(sign: true, limbs: y.limbs[(2 * a)..(n - 1)])
            am1: BigInt
            a1: BigInt
            am2: BigInt
            tmp: BigInt
            tmp2: BigInt
            z4: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        y0.removeLeadingZeros()
        y1.removeLeadingZeros()
        tmp = x0 + x2
        tmp2 = y0 + y2
        z0 = x0.toom3Mul(y0)
        a1 = (tmp + x1).toom3Mul(tmp2 + y1)
        am1 = (tmp - x1).toom3Mul(tmp2 - y1)
        am2 = (x2.mulInt(4) - x1.mulInt(2) + x0).toom3Mul(y2.mulInt(4) - y1.mulInt(2) + y0)
        z4 = x2.toom3Mul(y2)
        tmp = am2 + z0.mulInt(3)
        z1 = tmp + a1.mulInt(2)
        z1 = z1.divInt(6) - z4.mulInt(2) - am1
        z2 = -z0 - z4 + (a1 + am1).divInt(2)
        z3 = -tmp + a1 + am1.mulInt(3)
        z3 = z3.divInt(6) + z4.mulInt(2)       
        if z4.limbs != @[0'i64]:
            z4.limbs.insert(newSeq[int64](4 * a), 0)
        if z3.limbs != @[0'i64]:
            z3.limbs.insert(newSeq[int64](3 * a), 0)
        if z2.limbs != @[0'i64]:
            z2.limbs.insert(newSeq[int64](2 * a), 0)
        if z1.limbs != @[0'i64]:
            z1.limbs.insert(newSeq[int64](a), 0)
        result = z0 + z1 + z2 + z3 + z4
        result.sign = (x.sign == y.sign)

# Toom Cook 3 squaring.
# Evaluation points are infinity, 1, -1, -2 and 0.
proc toom3Sqr(x: BigInt): BigInt = 
    var
        m: int = len(x.limbs)
        a: int = m div 3
    if m < TOOM3_THRESHOLD:
        result = x.karatsubaSqr()
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(2 * a - 1)])
            x2: BigInt = BigInt(sign: true, limbs: x.limbs[(2 * a)..(m - 1)])
            am1: BigInt
            a1: BigInt
            am2: BigInt
            tmp: BigInt
            z4: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt        
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        tmp = x0 + x2
        z0 = x0.toom3Sqr()
        a1 = (tmp + x1).toom3Sqr()
        am1 = (tmp - x1).toom3Sqr()
        am2 = (x2.mulInt(4) - x1.mulInt(2) + x0).toom3Sqr()
        z4 = x2.toom3Sqr()
        tmp = am2 + z0.mulInt(3)
        z1 = tmp + a1.mulInt(2)
        z1 = z1.divInt(6) - z4.mulInt(2) - am1
        z2 = -z0 - z4 + (a1 + am1).divInt(2)
        z3 = -tmp + a1 + am1.mulInt(3)
        z3 = z3.divInt(6) + z4.mulInt(2)       
        if z4.limbs != @[0'i64]:
            z4.limbs.insert(newSeq[int64](4 * a), 0)
        if z3.limbs != @[0'i64]:
            z3.limbs.insert(newSeq[int64](3 * a), 0)
        if z2.limbs != @[0'i64]:
            z2.limbs.insert(newSeq[int64](2 * a), 0)
        if z1.limbs != @[0'i64]:
            z1.limbs.insert(newSeq[int64](a), 0)
        result = z0 + z1 + z2 + z3 + z4
        result.sign = true

# Toom Cook 4 multiplication. Time complexity is O(n^1.404).
# Evaluation points are infinity, 1, -1, 2, -2, -1/2 and 0.
proc toom4Mul(x, y: BigInt): BigInt = 
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        a: int = min(m, n) div 4
    if (m < TOOM4_THRESHOLD) or (n < TOOM4_THRESHOLD):
        result = x.toom3Mul(y)
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(2 * a - 1)])
            x2: BigInt = BigInt(sign: true, limbs: x.limbs[(2 * a)..(3 * a - 1)])
            x3: BigInt = BigInt(sign: true, limbs: x.limbs[(3 * a)..(m - 1)])
            y0: BigInt = BigInt(sign: true, limbs: y.limbs[0..(a - 1)])
            y1: BigInt = BigInt(sign: true, limbs: y.limbs[a..(2 * a - 1)])
            y2: BigInt = BigInt(sign: true, limbs: y.limbs[(2 * a)..(3 * a - 1)])
            y3: BigInt = BigInt(sign: true, limbs: y.limbs[(3 * a)..(n - 1)])
            amhalf: BigInt # a(-1/2)
            am1: BigInt # a(-1)
            a1: BigInt # a(1)
            am2: BigInt # a(-2)
            a2: BigInt # a(2)
            tmp: BigInt
            tmp2: BigInt
            tmp3: BigInt
            tmp4: BigInt
            z6: BigInt
            z5: BigInt
            z4: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt           
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        x2.removeLeadingZeros()
        y0.removeLeadingZeros()
        y1.removeLeadingZeros()
        y2.removeLeadingZeros()
        z0 = x0.toom4Mul(y0)
        z6 = x3.toom4Mul(y3)
        amhalf = (-x3 + x2.mulInt(2) - x1.mulInt(4) + x0.mulInt(8)).toom4Mul(-y3 + y2.mulInt(2) - y1.mulInt(4) + y0.mulInt(8))        
        tmp = x3.mulInt(8) + x1.mulInt(2)
        tmp2 = x2.mulInt(4) + x0
        tmp3 = y3.mulInt(8) + y1.mulInt(2)
        tmp4 = y2.mulInt(4) + y0
        a2 = (tmp + tmp2).toom4Mul(tmp3 + tmp4)
        am2 = (-tmp + tmp2).toom4Mul(-tmp3 + tmp4)
        tmp = x3 + x1
        tmp2 = x2 + x0
        tmp3 = y3 + y1
        tmp4 = y2 + y0
        a1 = (tmp + tmp2).toom4Mul(tmp3 + tmp4)
        am1 = (-tmp + tmp2).toom4Mul(-tmp3 + tmp4)
        tmp = z0 + z6
        tmp2 = a1 + am1
        tmp3 = am2.mulInt(5)
        tmp4 = a2.mulInt(3)
        z5 = tmp.mulInt(90) - amhalf.mulInt(2) + tmp4 - tmp3 - a1.mulInt(20) + am1.mulInt(60)
        z5 = z5.divInt(180)
        z1 = -amhalf.mulInt(8) - tmp4 - tmp3 + a1.mulInt(40) + am1.mulInt(120)
        z1 = z1.divInt(180) + tmp.mulInt(2)
        tmp3 = a2 + am2
        z4 = tmp3 - tmp2.mulInt(4) + z0.mulInt(6)
        z4 = z4.divInt(24) - z6.mulInt(5)
        z3 = -(tmp.mulInt(45)) + amhalf + am2 + a1.mulInt(7) - am1.mulInt(27)
        z3 = z3.divInt(18)
        z2 = -tmp3 + tmp2.mulInt(16) - z0.mulInt(30)
        z2 = z2.divInt(24) + z6.mulInt(4)
        z6.limbs.insert(newSeq[int64](6 * a), 0)
        if z5.limbs != @[0'i64]:
            z5.limbs.insert(newSeq[int64](5 * a), 0)
        if z4.limbs != @[0'i64]:
            z4.limbs.insert(newSeq[int64](4 * a), 0)
        if z3.limbs != @[0'i64]:
            z3.limbs.insert(newSeq[int64](3 * a), 0)
        if z2.limbs != @[0'i64]:
            z2.limbs.insert(newSeq[int64](2 * a), 0)
        if z1.limbs != @[0'i64]:
            z1.limbs.insert(newSeq[int64](a), 0)
        result = z0 + z1 + z2 + z3 + z4 + z5 + z6
        result.sign = (x.sign == y.sign)

# Toom Cook 4 squaring.
# Evaluation points are infinity, 1, -1, 2, -2, -1/2 and 0.
proc toom4Sqr(x: BigInt): BigInt = 
    var
        m: int = len(x.limbs)
        a: int = m div 4
    if m < TOOM4_THRESHOLD:
        result = x.toom3Sqr()
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(2 * a - 1)])
            x2: BigInt = BigInt(sign: true, limbs: x.limbs[(2 * a)..(3 * a - 1)])
            x3: BigInt = BigInt(sign: true, limbs: x.limbs[(3 * a)..(m - 1)])
            amhalf: BigInt # a(-1/2)
            am1: BigInt # a(-1)
            a1: BigInt # a(1)
            am2: BigInt # a(-2)
            a2: BigInt # a(2)
            tmp: BigInt
            tmp2: BigInt
            tmp3: BigInt
            z6: BigInt
            z5: BigInt
            z4: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt          
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        x2.removeLeadingZeros()
        z0 = x0.toom4Sqr()
        z6 = x3.toom4Sqr()
        amhalf = (-x3 + x2.mulInt(2) - x1.mulInt(4) + x0.mulInt(8)).toom4Sqr()        
        tmp = x3.mulInt(8) + x1.mulInt(2)
        tmp2 = x2.mulInt(4) + x0
        a2 = (tmp + tmp2).toom4Sqr()
        am2 = (-tmp + tmp2).toom4Sqr()
        tmp = x3 + x1
        tmp2 = x2 + x0
        a1 = (tmp + tmp2).toom4Sqr()
        am1 = (-tmp + tmp2).toom4Sqr()
        tmp = z0 + z6
        tmp2 = a2.mulInt(3)
        tmp3 = am2.mulInt(5)
        z5 = tmp.mulInt(90) - amhalf.mulInt(2) + tmp2 - tmp3 - a1.mulInt(20) + am1.mulInt(60)
        z5 = z5.divInt(180)
        z1 =  -amhalf.mulInt(8) - tmp2 - tmp3 + a1.mulInt(40) + am1.mulInt(120)
        z1 = z1.divInt(180) + tmp.mulInt(2)
        tmp2 = a1 + am1
        tmp3 = a2 + am2
        z4 = tmp3 - tmp2.mulInt(4) + z0.mulInt(6)
        z4 = z4.divInt(24) - z6.mulInt(5)
        z3 = -(tmp.mulInt(45)) + amhalf + am2 + a1.mulInt(7) - am1.mulInt(27)
        z3 = z3.divInt(18)
        z2 = -tmp3 + tmp2.mulInt(16) - z0.mulInt(30)
        z2 = z2.divInt(24) + z6.mulInt(4)
        z6.limbs.insert(newSeq[int64](6 * a), 0)
        if z5.limbs != @[0'i64]:
            z5.limbs.insert(newSeq[int64](5 * a), 0)
        if z4.limbs != @[0'i64]:
            z4.limbs.insert(newSeq[int64](4 * a), 0)
        if z3.limbs != @[0'i64]:
            z3.limbs.insert(newSeq[int64](3 * a), 0)
        if z2.limbs != @[0'i64]:
            z2.limbs.insert(newSeq[int64](2 * a), 0)
        if z1.limbs != @[0'i64]:
            z1.limbs.insert(newSeq[int64](a), 0)
        result = z0 + z1 + z2 + z3 + z4 + z5 + z6
        result.sign = true

# Multiplication. Used algorithms are changed with limbs length.
# Karatsuba and Toom Cook multiplication are slow when the sizes of two operands are different, 
# so filling zeros to smaller value.        
proc `*` *(x, y: BigInt): BigInt =
    var 
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        y2: BigInt
    if n > m:
        result = y * x
    elif n < KARATSUBA_THRESHOLD:
        if x == y:
            result = x.schoolbookSqr()
        else:
            result = x.schoolbookMul(y)
    elif n < TOOM3_THRESHOLD:
        if x == y:
            result = x.karatsubaSqr()
        elif n == m:
            result = x.karatsubaMul(y)
        else:
            y2 = BigInt(sign: y.sign, limbs: concat(repeat(0'i64, (m - n)), y.limbs))
            result = x.karatsubaMul(y2)
            result.limbs.delete(0,(m - n - 1))
    elif n < TOOM4_THRESHOLD:
        if x == y:
            result = x.toom3Sqr()
        elif n == m:
            result = x.toom3Mul(y)
        else:
            y2 = BigInt(sign: y.sign, limbs: concat(repeat(0'i64, (m - n)), y.limbs))
            result = x.toom3Mul(y2)
            result.limbs.delete(0,(m - n - 1))
    else:
        if x == y:
            result = x.toom4Sqr()
        elif n == m:
            result = x.toom4Mul(y)
        else:
            y2 = BigInt(sign: y.sign, limbs: concat(repeat(0'i64, (m - n)), y.limbs))
            result = x.toom4Mul(y2)
            result.limbs.delete(0,(m - n - 1))
            
proc `*` *(x: BigInt, y: SomeInteger): BigInt =
    result = x * newBigInt(y)

proc `*` *(x: SomeInteger, y: BigInt): BigInt =
    result = newBigInt(x) * y

proc `+=` *[T: SomeInteger|BigInt](x: var BigInt, y: T) =
    x = x + y

proc `-=` *[T: SomeInteger|BigInt](x: var BigInt, y: T) =
    x = x - y

proc `*=` *[T: SomeInteger|BigInt](x: var BigInt, y: T) =
    x = x * y

proc `^` *(x: BigInt, y: SomeInteger): BigInt =
    var
        t: BigInt
        s: string
        m: int 
    if y < 0:
        if abs(x) > newBigInt(1):
            result = zero
        elif abs(x) == newBigInt(1):
            result = x
        elif x == zero:
            raise newException(ValueError, "Division by zero.")
    elif y == 0:
        result = newBigInt(1)
    elif y == 1:
        result = x
    else:
        s = y.toBin(64)
        while ($s[0] == "0") and (len(s) > 1):
            s.delete(0, 0)
        m = len(s)
        s.delete(m, m)
        m = len(s)
        t = x
        result = newBigInt(1)
        for i in countdown(m-1, 0):
            if $s[i] == "1":
                result *= t
            t *= t

proc `^` *(x, y: BigInt): BigInt =
    if abs(y) > newBigInt("9223372036854775807"):
        raise newException(ValueError, "Exponent too large.")
    result = x^(($y).parseBiggestInt)

iterator `..` *(x, y: BigInt): BigInt =
    var t = x
    while t <= y:
        yield t
        t += 1

iterator `..<` *(x, y: BigInt): BigInt =
    var t = x
    while t < y:
        yield t
        t += 1

iterator countup*(x, y: BigInt, step: int = 1): BigInt =
    var t = x
    while t <= y:
        yield t
        t += step

iterator countdown*(x, y: BigInt, step: int = 1): BigInt =
    var t = x
    while t >= y:
        yield t
        t -= step

# BigFloat implementation

proc setPrec*(prec: int) = 
    bfContext.prec = prec

proc getPrec*(): int =
    result = bfContext.prec

proc truncate*(x: BigFloat): BigFloat =
    var 
        m: int = (bfContext.prec.toFloat / LOG_BASE.toFloat).ceil.toInt + 2
        n: int = len(x.intPart.limbs) - m 
    result = new BigFloat
    result.intPart = BigInt(sign: x.intPart.sign, limbs: x.intPart.limbs[max(0, n)..^1])
    result.exp = x.exp

proc truncateForString(x: BigFloat): BigFloat =
    var 
        m: int = (bfContext.prec.toFloat / LOG_BASE.toFloat).ceil.toInt + 1
        n: int = len(x.intPart.limbs) - m 
    result = new BigFloat
    result.intPart = BigInt(sign: x.intPart.sign, limbs: x.intPart.limbs[max(0, n)..^1])
    result.exp = x.exp

proc newBigFloat*(s: string, checkInput: bool = true): BigFloat =
    var
        s2: string
        resultSign: bool
        count: int = 1
        m: int
    if checkInput:
        var countPoints: int = 0
        var s3: string = s[0..^1]
        if ($s3[0] == "+") or ($s3[0] == "-"):
            s3.delete(0,0)
        for i in (0..<len(s3)):
            if $s3[i] == "+":
                raise newException(ValueError, "Not leading '+' in input string.")
            elif $s3[i] == "-":
                raise newException(ValueError, "Not leading '-' in input string.")
            elif not ($s3[i] in validCharsForBigFloat):
                raise newException(ValueError, "Invalid character(s) in input string.")
            elif $s3[i] == ".":
                countPoints += 1
        if countPoints > 1:
            raise newException(ValueError, "Input string includes multiple '.'.")
    result = new BigFloat
    if $s[0] == "-":
        s2 = s[1..^1]
        resultSign = false
    else:
        s2 = s[0..^1]
        resultSign = true
    if "." in s2:
        if $s2[0] == "0":
            s2 = s2[2..^1]
            while ($s2[0] == "0") and (len(s2) > 1):
                s2 = s2[1..^1]
                count += 1
            result.exp = -count
            result.intPart = newBigIntNoCheck(s2)
            result.intPart.sign = resultSign
            result = result.truncate()
        else:
            m = s2.find(".")
            s2.delete(m,m)
            result.exp = m - 1
            result.intPart = newBigIntNoCheck(s2)
            result.intPart.sign = resultSign
            result = result.truncate()
    else:
        result.exp = len(s2) - 1
        result.intPart = newBigIntNoCheck(s2)
        result.intPart.sign = resultSign
        result = result.truncate()

proc newBigFloat*(a: BigInt): BigFloat =
    result = new BigFloat
    result.intPart = a
    result.exp = LOG_BASE * (len(result.intPart.limbs) - 1) + len($result.intPart.limbs[^1]) - 1
    result = result.truncate()

proc newBigFloat*(a: SomeInteger): BigFloat =
    result = newBigFloat(newBigInt(a))

proc toStr*(x: BigFloat): string = 
    var
        s: string
        s2: string
        m: int
        x2: BigFloat = x.truncateForString()
    if x2.intPart == zero:
        result = "0"
    else:
        s = x2.intPart.abs().toStr()
        m = len(s)
        if x2.exp >= 0:
            if m > x2.exp + 1:
                s2 = s[0..x2.exp]
                s2.add(".")
                s2.add($s[(x2.exp+1)..^1])
                s = s2
            elif m < x2.exp + 1:
                s2 = $s[0]
                s2.add(".")
                s2.add($s[1..^1])
                if $s2[^1] == ".":
                    s2.add("0")
                s2.add("e")
                s2.add($x2.exp)
                s = s2
        else:
            if x2.exp >= -10:
                s2 = "0."
                s2.add("0".repeat(x2.exp.abs() - 1))
                s2.add(s)
                s = s2
            else:
                s2 = $s[0]
                s2.add(".")
                s2.add($s[1..^1])
                if $s2[^1] == ".":
                    s2.add("0")
                s2.add("e")
                s2.add($x2.exp)
                s = s2
        if x2.intPart.sign:
            result = s
        else:
            result = "-"
            result.add(s)

proc `$` *(x: BigFloat): string =
    result = x.toStr()

proc `-` *(x: BigFloat): BigFloat =
    result = new BigFloat
    if x.intPart == zero:
        result = x
    else:
        result.intPart = newBigInt(0)
        result.intPart.limbs = x.intPart.limbs[0..^1]
        result.exp = x.exp
        result.intPart.sign = not x.intPart.sign

proc abs*(x: BigFloat): BigFloat = 
    result = new BigFloat
    result.intPart = newBigInt(0)
    result.intPart.limbs = x.intPart.limbs[0..^1]
    result.intPart.sign = true
    result.exp = x.exp
    
proc `+` *(x, y: BigFloat): BigFloat =
    if x.intPart == zero:
        result = y
    elif y.intPart == zero:
        result = x
    elif y.exp > x.exp:
        result = y + x
    else:
        var
            n: int
            a: int
            b: int
            x2: BigInt
            y2: BigInt
            tmp: string
            zeros: seq[int64]
        n = bfContext.prec
        if x.exp - y.exp > n:
            result = x
        else:
            a = x.exp - LOG_BASE * (len(x.intPart.limbs) - 1) - len($(x.intPart.limbs[^1])) + 1
            b = y.exp - LOG_BASE * (len(y.intPart.limbs) - 1) - len($(y.intPart.limbs[^1])) + 1
            x2 = BigInt(sign: x.intPart.sign, limbs: x.intPart.limbs[0..^1])
            y2 = BigInt(sign: y.intPart.sign, limbs: y.intPart.limbs[0..^1])
            if a >= b:
                tmp = "1"
                tmp.add("0".repeat((a - b) mod LOG_BASE))
                x2 = x2 * newBigIntNoCheck(tmp)
                zeros = repeat(0'i64,((a - b) div LOG_BASE))
                x2.limbs.insert(zeros, 0)
            else:
                tmp = "1"
                tmp.add("0".repeat((b - a) mod LOG_BASE))
                y2 = y2 * newBigIntNoCheck(tmp)
                zeros = repeat(0'i64,((b - a) div LOG_BASE))
                y2.limbs.insert(zeros, 0)
            result = new BigFloat
            result.intPart = x2 + y2
            result.exp = x.exp
            if LOG_BASE * (len(result.intPart.limbs) - 1) + len($(result.intPart.limbs[^1])) != LOG_BASE * (len(x2.limbs) - 1) + len($(x2.limbs[^1])):
                result.exp += LOG_BASE * (len(result.intPart.limbs) - 1) + len($(result.intPart.limbs[^1])) - LOG_BASE * (len(x2.limbs) - 1) - len($(x2.limbs[^1]))
            result = result.truncate()

proc `+` *[T: SomeInteger|BigInt](x: BigFloat, y: T): BigFloat =
    result = x + newBigFloat(y)

proc `+` *[T: SomeInteger|BigInt](x: T, y: BigFloat): BigFloat =
    result = newBigFloat(x) + y

proc `-` *(x, y: BigFloat): BigFloat =
    result = x + (-y)

proc `-` *[T: SomeInteger|BigInt](x: BigFloat, y: T): BigFloat =
    result = x - newBigFloat(y)

proc `-` *[T: SomeInteger|BigInt](x: T, y: BigFloat): BigFloat =
    result = newBigFloat(x) - y

proc `*` *(x, y: BigFloat): BigFloat =
    if y.exp > x.exp:
        result = y * x
    elif (x.intPart == zero) or (y.intPart == zero):
        result = new BigFloat
        result.intPart = zero
        result.exp = 0
    else:
        result = new BigFloat
        result.intPart = x.intPart * y.intPart
        result.exp = x.exp + y.exp
        result.exp += LOG_BASE * (len(result.intPart.limbs) - 1) + len($(result.intPart.limbs[^1])) - LOG_BASE * (len(x.intPart.limbs) - 1) - len($(x.intPart.limbs[^1])) - LOG_BASE * (len(y.intPart.limbs) - 1) - len($(y.intPart.limbs[^1])) + 1
        result = result.truncate()

proc `*` *[T: SomeInteger|BigInt](x: BigFloat, y: T): BigFloat =
    result = x * newBigFloat(y)

proc `*` *[T: SomeInteger|BigInt](x: T, y: BigFloat): BigFloat =
    result = newBigFloat(x) * y

proc `>` *(x, y: BigFloat): bool = 
    return (x - y).intPart > zero

proc `>=` *(x, y: BigFloat): bool = 
    return (x - y).intPart >= zero

proc `==` *(x, y: BigFloat): bool = 
    return (x - y).intPart == zero

proc `<=` *(x, y: BigFloat): bool = 
    return (x - y).intPart <= zero

proc `<` *(x, y: BigFloat): bool = 
    return (x - y).intPart < zero

proc max*(x, y: BigFloat): BigFloat =
    if x >= y:
        result = x
    else:
        result = y

proc min*(x, y: BigFloat): BigFloat =
    if x < y:
        result = x
    else:
        result = y

# Inverse of x by Newton-Raphson method. Used for division.
proc inv(x: BigFloat): BigFloat =
    var 
        y: BigFloat
        ystring: string
        yfloat: float 
        one: BigFloat
        two: BigFloat
        x2: BigFloat
        t: int = (bfContext.prec * 51) div 100
        prec_list: seq[int] = @[t]
        precOrig: int = bfContext.prec
    while t >= 16:
        t = t div 2
        prec_list.add(t)
    prec_list.reverse
    setPrec(16)
    y = x.truncate()
    ystring = $y.intPart
    if len(ystring) > 10:
        ystring = ystring[0..9]
    yfloat = ystring.parseFloat()
    result = newBigFloat((1 / yfloat).formatFloat(format = ffDecimal, precision = 10))
    result.exp -= x.exp - len(ystring) + 1
    if $ystring[0] == "-":
        result.exp -= 1
    x2 = x.truncate()
    result = result.truncate()
    one = newBigFloat("1")
    two = newBigFloat("2")
    for i in 0..3:
        result = result + result * (one - x2 * result)
    for i in 0..(len(prec_list) - 1):
        t = prec_list[i] + 16
        setPrec(t)
        result = result.truncate()
        result = result + result * (one - x.truncate() * result)
    setPrec(precOrig + 16)
    result = result.truncate()
    result = result + result * (one - x * result)
    setPrec(precOrig)
    result = result.truncate()

proc `/` *(x, y: BigFloat): BigFloat =
    if y.intPart == zero:
        raise newException(ValueError, "Division by zero.")
    result = x * (y.inv())

proc `/` *[T:SomeInteger|BigInt](x: BigFloat, y: T): BigFloat =
    result = x / newBigFloat(y)

proc `/` *[T:SomeInteger|BigInt](x: T, y: BigFloat): BigFloat =
    result = newBigFloat(x) / y

proc `+=` *[T: BigFloat|BigInt|SomeInteger](x: var BigFloat, y: T) = 
    x = x + y

proc `-=` *[T: BigFloat|BigInt|SomeInteger](x: var BigFloat, y: T) = 
    x = x - y

proc `*=` *[T: BigFloat|BigInt|SomeInteger](x: var BigFloat, y: T) = 
    x = x * y

proc `/=` *[T: BigFloat|BigInt|SomeInteger](x: var BigFloat, y: T) = 
    x = x / y

# Square root of x by Newton-raphson method.
proc sqrt*(x: BigFloat): BigFloat = 
    var 
        y: BigFloat
        one: BigFloat
        half: BigFloat
        t: int = (bfContext.prec * 51) div 100
        prec_list: seq[int] = @[t]
        precOrig: int = bfContext.prec
    if not x.intPart.sign:
        raise newException(ValueError, "Negative value for sqrt is not supported.")
    while t >= 16:
        t = t div 2
        prec_list.add(t)
    prec_list.reverse
    setPrec(16)
    y = x.truncate()
    one = newBigFloat("1")
    half = newBigFloat("0.5")
    result = newBigFloat("1")
    result.exp += y.exp div 2
    for i in 0..12:
        result = (result * half) + (y * half * result.inv())
    result = result.inv()
    for i in 0..(len(prec_list) - 1):
        t = prec_list[i] + 16
        setPrec(t)
        result = result.truncate()
        result = result + (result * ((one - (x.truncate() * (result * result))) * half))
    setPrec(precOrig + 16)
    result = result.truncate()
    result = result + (result * (((one - (x * (result * result))).truncate()) * half))
    result = result * x
    setPrec(precOrig)
    result = result.truncate()

proc sqrt*(x: BigInt): BigFloat =
    result = newBigFloat(x).sqrt()

proc `^` *(x: BigFloat, y: SomeInteger): BigFloat =
    if y < 0:
        if x.intPart == zero:
            raise newException(ValueError, "Division by zero.")
        else:
            result = (x^abs(y)).inv()
    elif y == 0:
        result = BigFloat(intPart: zero, exp: 0)
    elif y == 1:
        result = x
    else:
        var
            s: string
            m: int
            t: BigFloat
        s = y.toBin(64)
        while ($s[0] == "0") and (len(s) > 1):
            s.delete(0, 0)
        m = len(s)
        s.delete(m, m)
        m = len(s)
        t = x
        result = newBigFloat("1")
        for i in countdown(m-1, 0):
            if $s[i] == "1":
                result *= t
            t *= t

proc `^` *(x: BigFloat, y: BigInt): BigFloat =
    if abs(y) > newBigInt("9223372036854775807"):
        raise newException(ValueError, "Exponent too large.")
    result = x^(($y).parseBiggestInt)

# BigInt division depends on BigFloat division, so implemented here.
# Not tested well. Might be incorrect for some corner cases.
proc `div` *(x, y: BigInt): BigInt =
    if x == zero:
        return zero
    else:
        var
            x2: BigInt = x.abs()
            y2: BigInt = y.abs()
        if x2 < y2:
            return zero
        elif x2 == y2:
            if x.sign == y.sign:
                result = newBigIntNoCheck("1")
            else:
                result = newBigIntNoCheck("-1")
        else:
            var 
                xfloat: BigFloat
                yfloat: BigFloat
                zfloat: BigFloat
                zstring: string
                m: int = 2 * (16 * len(x.limbs) + 16)
                precOrig: int = bfContext.prec
                eps: BigFloat
                n: int
            setPrec(m)
            eps = BigFloat(intPart: newBigIntNoCheck("1"), exp: -(m div 2))
            xfloat = newBigFloat(x2)
            yfloat = newBigFloat(y2)
            zfloat = (xfloat / yfloat) + eps
            zstring = $zfloat
            n = zstring.find(".") - 1
            setPrec(precOrig)
            result = newBigIntNoCheck(zstring[0..n])
            if x.sign != y.sign:
                result.sign = false
 
proc `div` *(x: BigInt, y: SomeInteger): BigInt =
    result = x div newBigInt(y)

proc `div` *(x: SomeInteger, y: BigInt): BigInt =
    result = newBigInt(x) div y

proc `mod` *(x, y: BigInt): BigInt =
    result = x - y * (x div y)

proc `mod` *(x: BigInt, y: SomeInteger): BigInt =
    result = x mod newBigInt(y)

proc `mod` *(x: SomeInteger, y: BigInt): BigInt =
    result = newBigInt(x) mod y

proc divmod*(x, y: BigInt): seq[BigInt] = 
    var t: BigInt 
    t = x div y
    result = @[t, x - y * t]

proc divmod*(x: BigInt, y: SomeInteger): seq[BigInt] =
    result = x.divmod(newBigInt(y))

proc divmod*(x: SomeInteger, y: BigInt): seq[BigInt] =
    result = newBigInt(x).divmod(y)
