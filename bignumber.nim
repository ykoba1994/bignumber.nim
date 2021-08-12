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
    KARATSUBA_THRESHOLD: int = 65  # The optimal value drastically changes with CPUs (from 30 to 200). 
    TOOM3_THRESHOLD: int = 350  # The optimal value <= 700. 
    TOOM4_THRESHOLD: int = 600  # The optimal value <= 1500. 
    TOOM6H_THRESHOLD: int = 900  # The optimal value <= 2000. 
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
        remainderLength: int
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
    remainderLength = inputLength mod LOG_BASE
    if remainderLength != 0:
        result.limbs.add(parseBiggestInt(s2[0..remainderLength - 1]))
        s2.delete(0,remainderLength - 1)
    for i in 0..(limbsLength - 1):
        result.limbs.add(parseBiggestInt(s2[LOG_BASE*i..(LOG_BASE*i + LOG_BASE-1)]))
    result.limbs.reverse
    # The sign of zero is always positive.
    if result.limbs == @[0'i64]:
        result.sign = true

proc newBigInt*(s: string, checkInput: bool = true): BigInt =
    ## Constructs a new BigInt object from a string.
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
    ## Constructs a new BigInt object from int64.
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
    if n >= BASE:
        result.limbs[0] = n mod BASE
        result.limbs.add(n div BASE)
    
proc newBigInt*[T: int8|uint8|int16|uint16|int32|uint32|int](a: T): BigInt =
    ## Constructs a new BigInt object from int8|uint8|int16|uint16|int32|uint32|int.
    result = newBigInt(int64(a))

proc newBigInt*(a: uint64): BigInt =
    ## Constructs a new BigInt object from uint64.
    result = newBigIntNoCheck($a)

proc toStr(x: BigInt): string = 
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
    if not x.sign:
        result = "-"
        result.add(s)
    else:
        result = s

proc `$` *(x: BigInt): string =
    ## Converts a BigInt object to a string.
    result = x.toStr()

proc ucmp(x, y: BigInt): int =
    let
        m: int = len(x.limbs)
        n: int = len(y.limbs)
    var
        a: int = 0
    if m > n:
        return 1
    elif m < n:
        return -1
    else:
        for i in countdown((m - 1),0):
            if x.limbs[i] > y.limbs[i]:
                a = 1
                break
            elif x.limbs[i] < y.limbs[i]:
                a = -1
                break
        return a

proc cmp(x, y: BigInt): int =
    if x.sign and (not y.sign):
        return 1
    elif (not x.sign) and y.sign:
        return -1
    elif x.sign and y.sign:
        return x.ucmp(y)
    else:
        return -(x.ucmp(y))

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
    ## Negates x.
    if x == zero:
        result = zero
    else:
        result = new BigInt
        result.sign = not x.sign
        result.limbs = x.limbs
    
proc abs*(x: BigInt): BigInt =
    ## Absolute value of x.
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
        carry: int64 = 0'i64
        t: int64
    result = new BigInt
    result.limbs = newSeqUninitialized[int64](m + 1)
    result.limbs[m] = 0
    result.sign = true
    for i in 0..(n - 1):
        t = x.limbs[i] + y.limbs[i] + carry
        if t >= BASE:
            t -= BASE 
            carry = 1 
        else:
            carry = 0
        result.limbs[i] = t
    for i in n..(m - 1):
        t = x.limbs[i] + carry
        if t >= BASE:
            t -= BASE 
            carry = 1
        else:
            carry = 0
        result.limbs[i] = t
    if carry == 1:
        result.limbs[m] = 1
    result.removeLeadingZeros()

# Unsigned subtraction. Only works when x >= y >= 0.
proc usub(x, y: BigInt): BigInt =
    var 
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        carry: int64 = 0'i64
        t: int64
    result = new BigInt    
    result.sign = true
    result.limbs = newSeqUninitialized[int64](m)
    for i in 0..(n - 1):
        t = x.limbs[i] - y.limbs[i] + carry
        if t < 0'i64:
            t += BASE
            carry = -1    
        else:
            carry = 0
        result.limbs[i] = t
    for i in n..(m - 1):
        t = x.limbs[i] + carry
        if t < 0'i64:
            t += BASE
            carry = -1    
        else:
            carry = 0
        result.limbs[i] = t
    result.removeLeadingZeros()

proc `+` *(x, y: BigInt): BigInt =
    ## Returns the sum of two BigInts.
    if x.ucmp(y) < 0:
        result = y + x
    else:
        if (x >= zero) and (y >= zero):
            result = x.uadd(y)
        elif (x < zero) and (y < zero):
            result = x.uadd(y)
            result.sign = false
        elif (x >= zero) and (y < zero):
            result = x.usub(y)
        else:
            result = x.usub(y)
            result.sign = false
            # The sign of zero is always positive.
            if result.limbs == @[0'i64]:
                result.sign = true

proc `+` *(x: BigInt, y: SomeInteger): BigInt =
    ## Returns the sum of a BigInt and an integer.
    result = x + newBigInt(y)

proc `+` *(x: SomeInteger, y: BigInt): BigInt =
    ## Returns the sum of an integer and a BigInt.
    result = newBigInt(x) + y

proc `-` *(x, y: BigInt): BigInt =
    ## Returns the difference of two BigInts.
    if x.ucmp(y) >= 0:
        if (x >= zero) and (y >= zero):
            result = x.usub(y)
        elif (x < zero) and (y < zero):
            result = x.usub(y)
            result.sign = false
        elif (x >= zero) and (y < zero):
            result = x.uadd(y)
        else:
            result = x.uadd(y)
            result.sign = false
            # The sign of zero is always positive.
            if result.limbs == @[0'i64]:
                result.sign = true
    else:
        if (y < zero) and (x < zero):
            result = y.usub(x)
        elif (y >= zero) and (x >= zero):
            result = y.usub(x)
            result.sign = false
        elif (y < zero) and (x >= zero):
            result = y.uadd(x)
        else:
            result = y.uadd(x)
            result.sign = false
            # The sign of zero is always positive.
            if result.limbs == @[0'i64]:
                result.sign = true

proc `-` *(x: BigInt, y: SomeInteger): BigInt =
    ## Returns the difference of a BigInt and an integer.
    result = x - newBigInt(y)

proc `-` *(x: SomeInteger, y: BigInt): BigInt =
    ## Returns the difference of an integer and a BigInt.
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
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
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

# Schoolbook multiplication with static array.
proc schoolbookMulStatic1(x, y: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: array[(KARATSUBA_THRESHOLD*5 div 8)*2, int64]
        yLimbsSplitted: array[(KARATSUBA_THRESHOLD*5 div 8)*2, int64]
        resultLimbsSplitted: array[(KARATSUBA_THRESHOLD*5 div 8)*4 + 2, int64]
    if (x == zero) or (y == zero):
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
        for i in 0..(n-1):
            t = y.limbs[i] div BASE2
            yLimbsSplitted[2*i] = y.limbs[i] - t*BASE2
            yLimbsSplitted[2*i + 1] = t
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

proc schoolbookMulStatic2(x, y: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: array[(KARATSUBA_THRESHOLD*3 div 4)*2, int64]
        yLimbsSplitted: array[(KARATSUBA_THRESHOLD*3 div 4)*2, int64]
        resultLimbsSplitted: array[(KARATSUBA_THRESHOLD*3 div 4)*4 + 2, int64]
    if (x == zero) or (y == zero):
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
        for i in 0..(n-1):
            t = y.limbs[i] div BASE2
            yLimbsSplitted[2*i] = y.limbs[i] - t*BASE2
            yLimbsSplitted[2*i + 1] = t
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

proc schoolbookMulStatic3(x, y: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: array[(KARATSUBA_THRESHOLD*7 div 8)*2, int64]
        yLimbsSplitted: array[(KARATSUBA_THRESHOLD*7 div 8)*2, int64]
        resultLimbsSplitted: array[(KARATSUBA_THRESHOLD*7 div 8)*4 + 2, int64]
    if (x == zero) or (y == zero):
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
        for i in 0..(n-1):
            t = y.limbs[i] div BASE2
            yLimbsSplitted[2*i] = y.limbs[i] - t*BASE2
            yLimbsSplitted[2*i + 1] = t
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

proc schoolbookMulStatic4(x, y: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: array[KARATSUBA_THRESHOLD*2, int64]
        yLimbsSplitted: array[KARATSUBA_THRESHOLD*2, int64]
        resultLimbsSplitted: array[KARATSUBA_THRESHOLD*4 + 2, int64]
    if (x == zero) or (y == zero):
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
        for i in 0..(n-1):
            t = y.limbs[i] div BASE2
            yLimbsSplitted[2*i] = y.limbs[i] - t*BASE2
            yLimbsSplitted[2*i + 1] = t
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
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
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

proc schoolbookSqrStatic1(x: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: array[(KARATSUBA_THRESHOLD*5 div 8)*2, int64]
        resultLimbsSplitted: array[(KARATSUBA_THRESHOLD*5 div 8)*4 + 2, int64]
    if x == zero:
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
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

proc schoolbookSqrStatic2(x: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: array[(KARATSUBA_THRESHOLD*3 div 4)*2, int64]
        resultLimbsSplitted: array[(KARATSUBA_THRESHOLD*3 div 4)*4 + 2, int64]
    if x == zero:
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
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

proc schoolbookSqrStatic3(x: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: array[(KARATSUBA_THRESHOLD*7 div 8)*2, int64]
        resultLimbsSplitted: array[(KARATSUBA_THRESHOLD*7 div 8)*4 + 2, int64]
    if x == zero:
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
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

proc schoolbookSqrStatic4(x: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        t: int64
        t2: int64
        xi: int64
        xLimbsSplitted: array[KARATSUBA_THRESHOLD*2, int64]
        resultLimbsSplitted: array[KARATSUBA_THRESHOLD*4 + 2, int64]
    if x == zero:
        result = new BigInt
        result.sign = true
        result.limbs = @[0'i64]
    else:
        for i in 0..(m-1):
            t = x.limbs[i] div BASE2
            xLimbsSplitted[2*i] = x.limbs[i] - t*BASE2
            xLimbsSplitted[2*i + 1] = t
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

# Unsigned destructive addition. Only works when x >= y >= 0. Used for Karatsuba and Toom-Cook multiplication.
proc udadd(x, y: var BigInt): BigInt =
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        carry: int64 = 0'i64
        t: int64
    x.limbs.setLen(m+1)
    for i in 0..(n - 1):
        t = x.limbs[i] + y.limbs[i] + carry
        if t >= BASE:
            t -= BASE 
            carry = 1
        else:
            carry = 0
        x.limbs[i] = t 
    for i in n..(m - 1):
        t = x.limbs[i] + carry
        if t >= BASE:
            t -= BASE 
            carry = 1
        else:
            carry = 0
        x.limbs[i] = t 
        if carry == 0:
            break
    if carry == 1:
        x.limbs[m] = 1
    x.removeLeadingZeros()
    result = x

# Unsigned destructive subtraction. Only works when x >= y >= 0. Used for Karatsuba and Toom-Cook multiplication.
proc udsub(x, y: var BigInt): BigInt =
    var 
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        carry: int64 = 0'i64
        t: int64
    for i in 0..(n - 1):
        t = x.limbs[i] - y.limbs[i] + carry
        if t < 0'i64:
            t += BASE
            carry = -1
        else:
            carry = 0
        x.limbs[i] = t    
    for i in n..(m - 1):
        t = x.limbs[i] + carry
        if t < 0'i64:
            t += BASE
            carry = -1
        else:
            carry = 0
        x.limbs[i] = t
        if carry == 0:
            break
    x.removeLeadingZeros()
    result = x

# Destructive negation.
proc dneg(x: var BigInt) =
    if x == zero:
        discard
    else:
        x.sign = not x.sign

# Destructive addition.
proc dadd(x, y: var BigInt): BigInt =
    #if abs(x) < abs(y):
    if x.ucmp(y) < 0:
        result = y.dadd(x)
    else:
        if (x >= zero) and (y >= zero):
            result = x.udadd(y)
        elif (x < zero) and (y < zero):
            result = x.udadd(y)
        elif (x >= zero) and (y < zero):
            result = x.udsub(y)
        else:
            result = x.udsub(y)
            # The sign of zero is always positive.
            if result.limbs == @[0'i64]:
                result.sign = true

# Destructive subtraction.
proc dsub(x, y: var BigInt): BigInt =
    y.dneg()
    result = x.dadd(y)

# Karatsuba multiplication. Time complexity is O(n^1.585).
proc karatsubaMul(x, y: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        a: int = min(m, n) div 2
    if min(m, n) < KARATSUBA_THRESHOLD:
        if max(m, n) < KARATSUBA_THRESHOLD*5 div 8:
            result = x.schoolbookMulStatic1(y)
        elif max(m, n) < KARATSUBA_THRESHOLD*3 div 4:
            result = x.schoolbookMulStatic2(y)
        elif max(m, n) < KARATSUBA_THRESHOLD*7 div 8:
            result = x.schoolbookMulStatic3(y)
        elif max(m, n) < KARATSUBA_THRESHOLD:
            result = x.schoolbookMulStatic4(y)
        else:
            result = x.schoolbookMul(y)
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(m - 1)])
            y0: BigInt = BigInt(sign: true, limbs: y.limbs[0..(a - 1)])
            y1: BigInt = BigInt(sign: true, limbs: y.limbs[a..(n - 1)])
            z1: BigInt
            z0: BigInt       
            tmp: BigInt  
            zeros: seq[int64] = newSeq[int64](a)
        x0.removeLeadingZeros()
        y0.removeLeadingZeros()
        result = x1.karatsubaMul(y1)
        z0 = x0.karatsubaMul(y0)
        tmp = (x1.dsub(x0)).karatsubaMul(y1.dsub(y0))
        x1 = zero
        x0 = zero
        y1 = zero
        y0 = zero
        z1 = result + z0
        z1 = z1.dsub(tmp)
        tmp = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z1)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z0)
        result.sign = (x.sign == y.sign)

# Karatsuba squaring.
proc karatsubaSqr(x: BigInt): BigInt =
    var
        m: int = len(x.limbs)
        a: int = m div 2
    if m < KARATSUBA_THRESHOLD:
        if m < KARATSUBA_THRESHOLD*5 div 8:
            result = x.schoolbookSqrStatic1()
        elif m < KARATSUBA_THRESHOLD*3 div 4:
            result = x.schoolbookSqrStatic2()
        elif m < KARATSUBA_THRESHOLD*7 div 8:
            result = x.schoolbookSqrStatic3()
        elif m < KARATSUBA_THRESHOLD:
            result = x.schoolbookSqrStatic4()
        else:
            result = x.schoolbookSqr()
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(m - 1)])
            z1: BigInt
            z0: BigInt
            tmp: BigInt
            zeros: seq[int64] = newSeq[int64](a)
        x0.removeLeadingZeros()
        result = x1.karatsubaSqr()
        z0 = x0.karatsubaSqr()
        z1 = result + z0
        tmp = (x1.dsub(x0)).karatsubaSqr()
        z1 = z1.dsub(tmp)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z1)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z0)
        result.sign = true

# Multiplication by small integers. Only used for Toom Cook multiplication.        
proc mulInt(x: BigInt, y: int64): BigInt =
    var 
        m: int = len(x.limbs)
        t: int64
        carry: int64 = 0
    result = new BigInt
    result.sign = x.sign
    result.limbs = newSeqUninitialized[int64](m)
    for i in 0..(m-1):
        t = x.limbs[i] * y + carry
        if t >= BASE:
            carry = t div BASE
            t -= carry * BASE
        else:
            carry = 0
        result.limbs[i] = t
    if carry > 0:
        result.limbs.setLen(m+1)
        result.limbs[m] = carry
    result.removeLeadingZeros()
    if result.limbs == @[0'i64]:
        result.sign = true
        
# Destructive mulInt.
proc dmulInt(x: var BigInt, y: int64): BigInt =
    var 
        m: int = len(x.limbs)
        t: int64
        carry: int64 = 0
    for i in 0..(m-1):
        t = x.limbs[i] * y + carry
        if t >= BASE:
            carry = t div BASE
            t -= carry * BASE
        else:
            carry = 0
        x.limbs[i] = t
    if carry > 0:
        x.limbs.setLen(m+1)
        x.limbs[m] = carry
    x.removeLeadingZeros()
    if x.limbs == @[0'i64]:
        x.sign = true
    result = x

# Division by small integers. Only used for Toom Cook multiplication. 
proc divInt(x: BigInt, y: int64): BigInt = 
    var 
        t: int64
        t2: int64
    result = new BigInt
    result.limbs = x.limbs[0..^1]
    result.sign = x.sign
    for i in countdown((len(x.limbs) - 1), 1):
        t = result.limbs[i]
        t2 = t div y
        result.limbs[i] = t2
        result.limbs[i-1] += (t - (t2 * y)) * BASE
    result.limbs[0] = result.limbs[0] div y
    result.removeLeadingZeros()
    if result.limbs == @[0'i64]:
        result.sign = true

# Destructive divInt.
proc ddivInt(x: var BigInt, y: int64): BigInt = 
    var 
        t: int64
        t2: int64
    for i in countdown((len(x.limbs) - 1), 1):
        t = x.limbs[i]
        t2 = t div y
        x.limbs[i] = t2
        x.limbs[i-1] += (t - (t2 * y)) * BASE
    x.limbs[0] = x.limbs[0] div y
    x.removeLeadingZeros()
    if x.limbs == @[0'i64]:
        x.sign = true
    result = x

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
            tmp3: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt
            zeros: seq[int64] = newSeq[int64](a)
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        y0.removeLeadingZeros()
        y1.removeLeadingZeros()
        tmp = x0 + x2
        tmp2 = y0 + y2
        z0 = x0.toom3Mul(y0)
        a1 = (tmp + x1).toom3Mul(tmp2 + y1)
        am1 = (tmp - x1).toom3Mul(tmp2 - y1)
        result = x2.toom3Mul(y2) 
        tmp = x2.dmulInt(4)
        tmp3 = x1.dmulInt(2)
        tmp = tmp.dsub(tmp3)
        tmp2 = y2.dmulInt(4)
        tmp3 = y1.dmulInt(2)
        tmp2 = tmp2.dsub(tmp3)
        am2 = (tmp.dadd(x0)).toom3Mul(tmp2.dadd(y0))
        tmp = am2 + z0.mulInt(3)
        z1 = tmp + a1.mulInt(2)
        z1 = z1.ddivInt(6)
        tmp3 = result.mulInt(2) + am1
        z1 = z1.dsub(tmp3)
        tmp2 = a1 + am1
        tmp2 = tmp2.ddivInt(2)
        z2 = -z0 - result + tmp2
        tmp = a1.dsub(tmp)
        z3 = am1.dmulInt(3)
        z3 = z3.dadd(tmp)
        z3 = z3.ddivInt(6)
        tmp3 = result.mulInt(2)
        z3 = z3.dadd(tmp3)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z3)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z2)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z1)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z0)
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
            tmp2: BigInt
            tmp3: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt
            zeros: seq[int64] = newSeq[int64](a)
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        tmp = x0 + x2
        z0 = x0.toom3Sqr()
        a1 = (tmp + x1).toom3Sqr()
        am1 = (tmp - x1).toom3Sqr()
        result = x2.toom3Sqr()
        tmp = x2.dmulInt(4)
        tmp3 = x1.dmulInt(2)
        tmp = tmp.dsub(tmp3)
        am2 = (tmp.dadd(x0)).toom3Sqr()
        tmp = am2 + z0.mulInt(3)
        z1 = tmp + a1.mulInt(2)
        z1 = z1.ddivInt(6)
        tmp3 = result.mulInt(2) + am1
        z1 = z1.dsub(tmp3)
        tmp2 = a1 + am1
        tmp2 = tmp2.ddivInt(2)
        z2 = -z0 - result + tmp2
        tmp = a1.dsub(tmp)
        z3 = am1.dmulInt(3)
        z3 = z3.dadd(tmp)
        z3 = z3.ddivInt(6)
        tmp3 = result.mulInt(2) 
        z3 = z3.dadd(tmp3)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z3)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z2)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z1)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z0)
        result.sign = true

# Toom Cook 4.5 multiplication. Time complexity is O(n^1.404).
# Evaluation points are 1, -1, 2, -2, 1/2, -1/2 and 0.
proc toom4hMul(x, y: BigInt): BigInt = 
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
            ahalf: BigInt
            am1: BigInt # a(-1)
            a1: BigInt # a(1)
            am2: BigInt # a(-2)
            a2: BigInt # a(2)
            tmp: BigInt
            tmp2: BigInt
            tmp3: BigInt
            tmp4: BigInt
            tmp5: BigInt
            tmp6: BigInt
            tmp7: BigInt
            z5: BigInt 
            z4: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt        
            zeros: seq[int64] = newSeq[int64](a)  
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        x2.removeLeadingZeros()
        y0.removeLeadingZeros()
        y1.removeLeadingZeros()
        y2.removeLeadingZeros()
        z0 = x0.toom4hMul(y0)
        tmp = x3.mulInt(2)
        tmp7 = x1.mulInt(8)
        tmp = tmp.dadd(tmp7)
        tmp2 = x2.mulInt(4)
        tmp7 = x0.mulInt(16)
        tmp2 = tmp2.dadd(tmp7)
        tmp3 = y3 + y1.mulInt(4)
        tmp4 = y2.mulInt(2)
        tmp7 = y0.mulInt(8)
        tmp4 = tmp4.dadd(tmp7)
        ahalf = (tmp2 + tmp).toom4hMul(tmp4 + tmp3)
        amhalf = (tmp2.dsub(tmp)).toom4hMul(tmp4.dsub(tmp3))
        tmp = x3.mulInt(8)
        tmp7 = x1.mulInt(2)
        tmp = tmp.dadd(tmp7)
        tmp2 = x2.mulInt(4) + x0
        tmp3 = y3.mulInt(8)
        tmp7 = y1.mulInt(2)
        tmp3 = tmp3.dadd(tmp7)
        tmp4 = y2.mulInt(4) + y0
        a2 = (tmp2 + tmp).toom4hMul(tmp4 + tmp3)
        am2 = (tmp2.dsub(tmp)).toom4hMul(tmp4.dsub(tmp3))
        tmp = x3.dadd(x1)
        tmp2 = x2.dadd(x0)
        tmp3 = y3.dadd(y1)
        tmp4 = y2.dadd(y0)
        x0 = zero
        x1 = zero
        x2 = zero
        x3 = zero
        y0 = zero
        y1 = zero
        y2 = zero
        y3 = zero
        a1 = (tmp2 + tmp).toom4hMul(tmp4 + tmp3)
        am1 = (tmp2.dsub(tmp)).toom4hMul(tmp4.dsub(tmp3))
        tmp = ahalf + amhalf
        tmp2 = ahalf.dsub(amhalf)
        tmp3 = a2 + am2
        tmp4 = a2.dsub(am2)
        tmp5 = a1 + am1
        tmp6 = a1.dsub(am1)
        a1 = zero
        am1 = zero
        a2 = zero
        am2 = zero
        ahalf = zero
        amhalf = zero
        #result = tmp + tmp3.mulInt(2) - tmp5.mulInt(40)
        result = tmp + tmp3.mulInt(2)
        tmp7 = tmp5.mulInt(40)
        result = result.dsub(tmp7)
        result = result.ddivInt(180) - z0
        #z5 = tmp2 + tmp4.mulInt(8) - tmp6.mulInt(80)
        z5 = tmp4.mulInt(8) + tmp2
        tmp7 = tmp6.mulInt(80)
        z5 = z5.dsub(tmp7)
        z5 = z5.ddivInt(360)
        #z4 = -tmp.mulInt(2) - tmp3 + tmp5.mulInt(68) + z0.mulInt(378)
        z4 = tmp5.mulInt(68) - tmp3
        tmp7 = tmp.mulInt(2)
        z4 = z4.dsub(tmp7)
        tmp7 = z0.mulInt(378)
        z4 = z4.dadd(tmp7)
        z4 = z4.ddivInt(72)
        #z3 = -tmp2 - tmp4.mulInt(2) + tmp6.mulInt(68)
        z3 = tmp6.mulInt(68) - tmp2
        tmp7 = tmp4.mulInt(2)
        z3 = z3.dsub(tmp7)
        z3 = z3.ddivInt(72)
        #z2 = tmp.mulInt(8) + tmp3 - tmp5.mulInt(80) - z0.mulInt(90).mulInt(21)
        z2 = tmp.dmulInt(8)
        z2 = z2.dadd(tmp3)
        tmp7 = tmp5.dmulInt(80)
        z2 = z2.dsub(tmp7)
        tmp7 = z0.mulInt(90)
        tmp7 = tmp7.dmulInt(21)
        z2 = z2.dsub(tmp7)
        z2 = z2.ddivInt(360)
        #z1 = tmp2.mulInt(2) + tmp4 - tmp6.mulInt(40)
        z1 = tmp2.dmulInt(2)
        z1 = z1.dadd(tmp4)
        tmp7 = tmp6.dmulInt(40)
        z1 = z1.dsub(tmp7)
        z1 = z1.ddivInt(180)
        tmp = zero
        tmp2 = zero
        tmp3 = zero
        tmp4 = zero
        tmp5 = zero
        tmp6 = zero
        tmp7 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z5)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z4)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z3)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z2)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z1)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z0)
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
            tmp4: BigInt
            tmp5: BigInt
            z5: BigInt
            z4: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt         
            zeros: seq[int64] = newSeq[int64](a)  
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        x2.removeLeadingZeros()
        z0 = x0.toom4Sqr()
        result = x3.toom4Sqr()
        tmp = x3.mulInt(8)
        tmp5 = x1.mulInt(2)
        tmp = tmp.dadd(tmp5)
        tmp2 = x2.mulInt(4) + x0
        a2 = (tmp + tmp2).toom4Sqr()
        am2 = (tmp2.dsub(tmp)).toom4Sqr()
        tmp = x3 + x1
        tmp2 = x2 + x0
        a1 = (tmp + tmp2).toom4Sqr()
        am1 = (tmp2.dsub(tmp)).toom4Sqr()
        tmp = x2.dmulInt(2)
        tmp = tmp.dsub(x3)
        tmp3 = x1.dmulInt(4)
        tmp = tmp.dsub(tmp3)
        tmp3 = x0.dmulInt(8)
        tmp = tmp.dadd(tmp3)
        amhalf = tmp.toom4Sqr()
        tmp = z0 + result
        tmp2 = a1 + am1
        tmp3 = am2.mulInt(5)
        tmp4 = a2.mulInt(3)
        z5 = tmp.mulInt(90)
        tmp5 = amhalf.mulInt(2)
        z5 = z5.dsub(tmp5)
        tmp5 = tmp4 - tmp3
        z5 = z5.dadd(tmp5)
        tmp5 = a1.mulInt(20)
        z5 = z5.dsub(tmp5)
        tmp5 = am1.mulInt(60)
        z5 = z5.dadd(tmp5)
        z5 = z5.divInt(180)
        z1 = -amhalf.mulInt(8)
        tmp5 = tmp4 + tmp3
        z1 = z1.dsub(tmp5)
        tmp5 = a1.mulInt(40)
        z1 = z1.dadd(tmp5)
        tmp5 = am1.mulInt(120)
        z1 = z1.dadd(tmp5)
        z1 = z1.ddivInt(180)
        tmp5 = tmp.mulInt(2)
        z1 = z1.dadd(tmp5)
        tmp3 = a2 + am2
        tmp5 = tmp2.mulInt(4)
        z4 = tmp3 - tmp5
        tmp5 = z0.mulInt(6)
        z4 = z4.dadd(tmp5)
        tmp5 = result.mulInt(5)
        z4 = z4.ddivInt(24)
        z4 = z4.dsub(tmp5)
        tmp = tmp.dmulInt(45)
        z3 = amhalf.dsub(tmp)
        z3 = z3.dadd(am2)
        tmp = a1.dmulInt(7)
        z3 = z3.dadd(tmp)
        tmp = am1.dmulInt(27)
        z3 = z3.dsub(tmp)
        z3 = z3.ddivInt(18)
        tmp = z0.mulInt(30)
        z2 = tmp2.dmulInt(16)
        z2 = z2.dsub(tmp3)
        z2 = z2.dsub(tmp)
        tmp = result.mulInt(4)
        z2 = z2.ddivInt(24)
        z2 = z2.dadd(tmp)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z5)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z4)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z3)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z2)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z1)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z0)
        result.sign = true

proc toom6hMul(x, y: BigInt): BigInt = 
    var
        m: int = len(x.limbs)
        n: int = len(y.limbs)
        a: int = min(m, n) div 6
    if (m < TOOM6H_THRESHOLD) or (n < TOOM6H_THRESHOLD):
        #result = x.toom3Mul(y)
        result = x.toom4hMul(y)
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(2 * a - 1)])
            x2: BigInt = BigInt(sign: true, limbs: x.limbs[(2 * a)..(3 * a - 1)])
            x3: BigInt = BigInt(sign: true, limbs: x.limbs[(3 * a)..(4 * a - 1)])
            x4: BigInt = BigInt(sign: true, limbs: x.limbs[(4 * a)..(5 * a - 1)])
            x5: BigInt = BigInt(sign: true, limbs: x.limbs[(5 * a)..(m - 1)])
            y0: BigInt = BigInt(sign: true, limbs: y.limbs[0..(a - 1)])
            y1: BigInt = BigInt(sign: true, limbs: y.limbs[a..(2 * a - 1)])
            y2: BigInt = BigInt(sign: true, limbs: y.limbs[(2 * a)..(3 * a - 1)])
            y3: BigInt = BigInt(sign: true, limbs: y.limbs[(3 * a)..(4 * a - 1)])
            y4: BigInt = BigInt(sign: true, limbs: y.limbs[(4 * a)..(5 * a - 1)])
            y5: BigInt = BigInt(sign: true, limbs: y.limbs[(5 * a)..(n - 1)])
            b: BigInt
            c: BigInt
            d: BigInt
            e: BigInt
            f: BigInt
            g: BigInt
            h: BigInt
            i: BigInt
            j: BigInt
            k: BigInt
            tmp: BigInt
            tmp2: BigInt
            tmp3: BigInt
            tmp4: BigInt
            tmp5: BigInt
            tmp6: BigInt
            tmp7: BigInt
            tmp8: BigInt
            tmp9: BigInt
            tmp10: BigInt
            tmp11: BigInt
            tmp12: BigInt
            z9: BigInt
            z8: BigInt
            z7: BigInt
            z6: BigInt
            z5: BigInt
            z4: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt         
            zeros: seq[int64] = newSeq[int64](a)
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        x2.removeLeadingZeros()
        x3.removeLeadingZeros()
        x4.removeLeadingZeros()
        y0.removeLeadingZeros()
        y1.removeLeadingZeros()
        y2.removeLeadingZeros()
        y3.removeLeadingZeros()
        y4.removeLeadingZeros()
        z0 = x0.toom6hMul(y0)
        #tmp = x5.mulInt(3) + x3.mulInt(27) + x1.mulInt(243)
        tmp = x5.mulInt(3)
        tmp11 = x3.mulInt(27)
        tmp = tmp.dadd(tmp11)
        tmp11 = x1.mulInt(243)
        tmp = tmp.dadd(tmp11)
        #tmp2 = x4.mulInt(9) + x2.mulInt(81) + x0.mulInt(729)
        tmp2 = x4.mulInt(9)
        tmp11 = x2.mulInt(81)
        tmp2 = tmp2.dadd(tmp11)
        tmp11 = x0.mulInt(729)
        tmp2 = tmp2.dadd(tmp11)
        #tmp3 = y5 + y3.mulInt(9) + y1.mulInt(81)
        tmp3 = y5 + y3.mulInt(9)
        tmp11 = y1.mulInt(81)
        tmp3 = tmp3.dadd(tmp11)
        #tmp4 = y4.mulInt(3) + y2.mulInt(27) + y0.mulInt(243)
        tmp4 = y4.mulInt(3)
        tmp11 = y2.mulInt(27)
        tmp4 = tmp4.dadd(tmp11)
        tmp11 = y0.mulInt(243)
        tmp4 = tmp4.dadd(tmp11)
        b = (tmp + tmp2).toom6hMul(tmp3 + tmp4)
        c = (tmp2.dsub(tmp)).toom6hMul(tmp4.dsub(tmp3))
        #tmp = x5.mulInt(243) + x3.mulInt(27) + x1.mulInt(3)
        tmp = x5.mulInt(243)
        tmp11 = x3.mulInt(27)
        tmp = tmp.dadd(tmp11)
        tmp11 = x1.mulInt(3)
        tmp = tmp.dadd(tmp11)
        #tmp2 = x4.mulInt(81) + x2.mulInt(9) + x0
        tmp2 = x4.mulInt(81)
        tmp11 = x2.mulInt(9) + x0
        tmp2 = tmp2.dadd(tmp11)
        #tmp3 = y5.mulInt(243) + y3.mulInt(27) + y1.mulInt(3)
        tmp3 = y5.mulInt(243)
        tmp11 = y3.mulInt(27)
        tmp3 = tmp3.dadd(tmp11)
        tmp11 = y1.mulInt(3)
        tmp3 = tmp3.dadd(tmp11)
        #tmp4 = y4.mulInt(81) + y2.mulInt(9) + y0
        tmp4 = y4.mulInt(81)
        tmp11 = y2.mulInt(9) + y0
        tmp4 = tmp4.dadd(tmp11)
        d = (tmp + tmp2).toom6hMul(tmp3 + tmp4)
        e = (tmp2.dsub(tmp)).toom6hMul(tmp4.dsub(tmp3))
        tmp = x5.mulInt(2)
        tmp11 = x3.mulInt(8)
        tmp = tmp.dadd(tmp11)
        tmp11 = x1.mulInt(32)
        tmp = tmp.dadd(tmp11)
        #tmp2 = x4.mulInt(4) + x2.mulInt(16) + x0.mulInt(64)
        tmp2 = x4.mulInt(4)
        tmp11 = x2.mulInt(16)
        tmp2 = tmp2.dadd(tmp11)
        tmp11 = x0.mulInt(64)
        tmp2 = tmp2.dadd(tmp11)
        #tmp3 = y5 + y3.mulInt(4) + y1.mulInt(16)
        tmp3 = y5 + y3.mulInt(4)
        tmp11 = y1.mulInt(16)
        tmp3 = tmp3.dadd(tmp11)
        #tmp4 = y4.mulInt(2) + y2.mulInt(8) + y0.mulInt(32)
        tmp4 = y4.mulInt(2)
        tmp11 = y2.mulInt(8)
        tmp4 = tmp4.dadd(tmp11)
        tmp11 = y0.mulInt(32)
        tmp4 = tmp4.dadd(tmp11)
        f = (tmp2 + tmp).toom6hMul(tmp4 + tmp3)
        g = (tmp2.dsub(tmp)).toom6hMul(tmp4.dsub(tmp3))
        #tmp = x5.mulInt(32) + x3.mulInt(8) + x1.mulInt(2)
        tmp = x5.mulInt(32)
        tmp11 = x3.mulInt(8)
        tmp = tmp.dadd(tmp11)
        tmp11 = x1.mulInt(2)
        tmp = tmp.dadd(tmp11)
        #tmp2 = x4.mulInt(16) + x2.mulInt(4) + x0
        tmp2 = x4.mulInt(16) + x0
        tmp11 = x2.mulInt(4)
        tmp2 = tmp2.dadd(tmp11)
        #tmp3 = y5.mulInt(32) + y3.mulInt(8) + y1.mulInt(2)
        tmp3 = y5.mulInt(32)
        tmp11 = y3.mulInt(8)
        tmp3 = tmp3.dadd(tmp11)
        tmp11 = y1.mulInt(2)
        tmp3 = tmp3.dadd(tmp11)
        #tmp4 = y4.mulInt(16) + y2.mulInt(4) + y0
        tmp4 = y4.mulInt(16) + y0
        tmp11 = y2.mulInt(4)
        tmp4 = tmp4.dadd(tmp11)
        h = (tmp2 + tmp).toom6hMul(tmp4 + tmp3)
        i = (tmp2.dsub(tmp)).toom6hMul(tmp4.dsub(tmp3))
        tmp = x5.dadd(x3)
        tmp = tmp.dadd(x1)
        tmp2 = x4.dadd(x2)
        tmp2 = tmp2.dadd(x0)
        tmp3 = y5.dadd(y3)
        tmp3 = tmp3.dadd(y1)
        tmp4 = y4.dadd(y2)
        tmp4 = tmp4.dadd(y0)
        x5 = zero
        x4 = zero
        x3 = zero
        x2 = zero
        x1 = zero
        x0 = zero
        y5 = zero
        y4 = zero
        y3 = zero
        y2 = zero
        y1 = zero
        y0 = zero
        j = (tmp2 + tmp).toom6hMul(tmp4 + tmp3)
        k = (tmp2.dsub(tmp)).toom6hMul(tmp4.dsub(tmp3))
        tmp = j + k
        tmp = tmp.dmulInt(50)
        tmp2 = j.dsub(k)
        tmp2 = tmp2.dmulInt(50)
        tmp3 = h + i
        tmp3 = tmp3.dmulInt(8)
        tmp4 = h.dsub(i)
        tmp4 = tmp4.dmulInt(16)
        tmp5 = f + g
        tmp5 = tmp5.dmulInt(16)
        tmp6 = f.dsub(g)
        tmp6 = tmp6.dmulInt(8) 
        tmp7 = d + e
        tmp8 = d.dsub(e)
        tmp9 = b + c
        tmp10 = b.dsub(c)
        b = zero
        c = zero
        d = zero
        e = zero
        f = zero
        g = zero
        h = zero
        i = zero
        j = zero
        k = zero
        #result = tmp.mulInt(105) - tmp3.mulInt(12) - tmp5.mulInt(3) + tmp7.mulInt(3) + tmp9
        result = tmp.mulInt(105)
        tmp11 = tmp3.mulInt(12)
        result = result.dsub(tmp11)
        #tmp11 = tmp5.mulInt(3)
        #result = result.dsub(tmp11)
        #tmp11 = tmp7.mulInt(3) + tmp9
        #result = result.dadd(tmp11)
        tmp11 = tmp7 - tmp5
        tmp11 = tmp11.dmulInt(3) + tmp9
        result = result.dadd(tmp11)
        result = result.ddivInt(480)
        result = result.ddivInt(350) - z0
        #z9 = tmp2.mulInt(315) - tmp4.mulInt(36) - tmp6.mulInt(9) + tmp8.mulInt(27) + tmp10
        z9 = tmp2.mulInt(315)
        tmp11 = tmp4.mulInt(36)
        z9 = z9.dsub(tmp11)
        tmp11 = tmp6.mulInt(9)
        z9 = z9.dsub(tmp11)
        tmp11 = tmp8.mulInt(27) + tmp10
        z9 = z9.dadd(tmp11)
        z9 = z9.ddivInt(875)
        z9 = z9.ddivInt(576)
        #z5 = tmp2.mulInt(733) - tmp4.mulInt(26) - tmp6.mulInt(26) + tmp8.mulInt(9) + tmp10.mulInt(3)
        z5 = tmp2.mulInt(733)
        tmp11 = tmp4 + tmp6
        tmp11 = tmp11.dmulInt(26)
        z5 = z5.dsub(tmp11)
        tmp11 = tmp8.mulInt(9)
        z5 = z5.dadd(tmp11)
        tmp11 = tmp10.mulInt(3)
        z5 = z5.dadd(tmp11)
        z5 = z5.ddivInt(288)
        z5 = z5.ddivInt(100)
        #z1 = tmp2.mulInt(105) - tmp4.mulInt(3) + tmp10.mulInt(3) + tmp8 - tmp6.mulInt(12) 
        z1 = tmp2.mulInt(105)
        #tmp11 = tmp4.mulInt(3)
        #z1 = z1.dsub(tmp11)
        #tmp11 = tmp6.mulInt(12)
        #z1 = z1.dsub(tmp11)
        #tmp11 = tmp8 + tmp10.mulInt(3)
        #z1 = z1.dadd(tmp11)
        tmp11 = tmp10 - tmp4
        tmp11 = tmp11.dmulInt(3)
        z1 = z1.dadd(tmp11) + tmp8
        tmp11 = tmp6.mulInt(12)
        z1 = z1.dsub(tmp11)
        z1 = z1.ddivInt(480)
        z1 = z1.ddivInt(350)
        tmp2.dneg()
        tmp2 = tmp2.dmulInt(35)
        tmp2 = tmp2.dmulInt(481)
        tmp4 = tmp4.dmulInt(2)
        tmp6 = tmp6.dmulInt(2)
        #z7 = tmp2 + tmp4.mulInt(746) + tmp6.mulInt(254) - tmp8.mulInt(579) - tmp10.mulInt(57)
        z7 = tmp2 + tmp4.mulInt(746)
        tmp11 = tmp6.mulInt(254)
        z7 = z7.dadd(tmp11)
        tmp11 = tmp8.mulInt(579)
        z7 = z7.dsub(tmp11)
        tmp11 = tmp10.mulInt(57)
        z7 = z7.dsub(tmp11)
        z7 = z7.ddivInt(768)
        z7 = z7.ddivInt(525)
        z7 = z7.ddivInt(5)
        #z3 = tmp2 + tmp4.mulInt(254) + tmp6.mulInt(746) - tmp8.mulInt(171) - tmp10.mulInt(193)
        tmp11 = tmp4.dmulInt(254)
        z3 = tmp2.dadd(tmp11)
        tmp11 = tmp6.dmulInt(746)
        z3 = z3.dadd(tmp11)
        tmp11 = tmp8.dmulInt(171)
        z3 = z3.dsub(tmp11)
        tmp11 = tmp10.dmulInt(193)
        z3 = z3.dsub(tmp11)
        z3 = z3.ddivInt(768)
        z3 = z3.ddivInt(525)
        z3 = z3.ddivInt(5)
        tmp2 = zero
        tmp4 = zero
        tmp6 = zero
        tmp8 = zero
        tmp10 = zero
        tmp5 = tmp5.dmulInt(2)
        tmp12 = z0.mulInt(400)
        #z2 = -z0.mulInt(400).mulInt(35).mulInt(517) + tmp.mulInt(315) - tmp3.mulInt(9) - tmp5.mulInt(18) + tmp7 + tmp9.mulInt(27)
        #z2 = -z0.mulInt(400)
        z2 = -tmp12.mulInt(35)
        z2 = z2.dmulInt(517)
        tmp11 = tmp.mulInt(315)
        z2 = z2.dadd(tmp11)
        tmp11 = tmp3.mulInt(9)
        z2 = z2.dsub(tmp11)
        tmp11 = tmp5.mulInt(18)
        z2 = z2.dsub(tmp11)
        tmp11 = tmp7 + tmp9.mulInt(27)
        z2 = z2.dadd(tmp11)
        z2 = z2.ddivInt(875)
        z2 = z2.ddivInt(576)
        tmp3 = tmp3.dmulInt(2)
        #z6 = -z0.mulInt(160).mulInt(649).mulInt(15) + tmp.mulInt(733) - tmp3.mulInt(13) - tmp5.mulInt(13) + tmp7.mulInt(3) + tmp9.mulInt(9)
        #z6 = -z0.mulInt(160)
        z6 = -tmp12.mulInt(6)
        z6 = z6.dmulInt(649)
        tmp11 = tmp.mulInt(733)
        z6 = z6.dadd(tmp11)
        tmp11 = tmp3 + tmp5
        tmp11 = tmp11.dmulInt(13)
        z6 = z6.dsub(tmp11)
        tmp11 = tmp7.mulInt(3)
        z6 = z6.dadd(tmp11)
        tmp11 = tmp9.mulInt(9)
        z6 = z6.dadd(tmp11)
        z6 = z6.ddivInt(288)
        z6 = z6.ddivInt(100)
        tmp = tmp.dmulInt(35)
        tmp = tmp.dmulInt(481)
        #z8 = z0.mulInt(320).mulInt(175).mulInt(517) - tmp + tmp3.mulInt(746) + tmp5.mulInt(254) - tmp7.mulInt(193) - tmp9.mulInt(171)
        #z8 = z0.mulInt(320)
        z8 = tmp12.mulInt(140)
        z8 = z8.dmulInt(517)
        tmp11 = tmp3.mulInt(746) - tmp
        z8 = z8.dadd(tmp11)
        tmp11 = tmp5.mulInt(254)
        z8 = z8.dadd(tmp11)
        tmp11 = tmp7.mulInt(193)
        z8 = z8.dsub(tmp11)
        tmp11 = tmp9.mulInt(171)
        z8 = z8.dsub(tmp11)
        z8 = z8.ddivInt(768)
        z8 = z8.ddivInt(525)
        z8 = z8.ddivInt(5)
        #z4 = z0.mulInt(480).mulInt(350).mulInt(649) - tmp + tmp3.mulInt(254) + tmp5.mulInt(746) - tmp7.mulInt(57) - tmp9.mulInt(579)
        #z4 = z0.mulInt(480)
        z4 = tmp12.dmulInt(420)
        z4 = z4.dmulInt(649)
        z4 = z4.dsub(tmp)
        tmp11 = tmp3.dmulInt(254)
        z4 = z4.dadd(tmp11)
        tmp11 = tmp5.dmulInt(746) 
        z4 = z4.dadd(tmp11)
        tmp11 = tmp7.dmulInt(57)
        z4 = z4.dsub(tmp11)
        tmp11 = tmp9.dmulInt(579)
        z4 = z4.dsub(tmp11)
        z4 = z4.ddivInt(768)
        z4 = z4.ddivInt(525)
        z4 = z4.ddivInt(5)
        tmp = zero
        tmp3 = zero
        tmp5 = zero
        tmp7 = zero
        tmp9 = zero
        tmp11 = zero
        tmp12 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z9)
        z9 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z8)
        z8 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z7)
        z7 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z6)
        z6 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z5)
        z5 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z4)
        z4 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z3)
        z3 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z2)
        z2 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z1)
        z1 = zero
        result.limbs.insert(zeros, 0)
        result = result.udadd(z0)
        z0 = zero
        result.sign = (x.sign == y.sign)
        
proc toom6hSqr(x: BigInt): BigInt = 
    var
        m: int = len(x.limbs)
        a: int = m div 6
    if m < TOOM6H_THRESHOLD:
        result = x.toom4Sqr()
    else:
        var
            x0: BigInt = BigInt(sign: true, limbs: x.limbs[0..(a - 1)])
            x1: BigInt = BigInt(sign: true, limbs: x.limbs[a..(2 * a - 1)])
            x2: BigInt = BigInt(sign: true, limbs: x.limbs[(2 * a)..(3 * a - 1)])
            x3: BigInt = BigInt(sign: true, limbs: x.limbs[(3 * a)..(4 * a - 1)])
            x4: BigInt = BigInt(sign: true, limbs: x.limbs[(4 * a)..(5 * a - 1)])
            x5: BigInt = BigInt(sign: true, limbs: x.limbs[(5 * a)..(m - 1)])
            b: BigInt
            c: BigInt
            d: BigInt
            e: BigInt
            f: BigInt
            g: BigInt
            h: BigInt
            i: BigInt
            j: BigInt
            k: BigInt
            tmp: BigInt
            tmp2: BigInt
            tmp3: BigInt
            tmp4: BigInt
            tmp5: BigInt
            tmp6: BigInt
            tmp7: BigInt
            tmp8: BigInt
            tmp9: BigInt
            tmp10: BigInt
            tmp11: BigInt
            z9: BigInt
            z8: BigInt
            z7: BigInt
            z6: BigInt
            z5: BigInt
            z4: BigInt
            z3: BigInt
            z2: BigInt
            z1: BigInt
            z0: BigInt         
            zeros: seq[int64] = newSeq[int64](a)
        x0.removeLeadingZeros()
        x1.removeLeadingZeros()
        x2.removeLeadingZeros()
        x3.removeLeadingZeros()
        x4.removeLeadingZeros()
        z0 = x0.toom6hSqr()
        #tmp = x5.mulInt(3) + x3.mulInt(27) + x1.mulInt(243)
        tmp = x5.mulInt(3)
        tmp11 = x3.mulInt(27)
        tmp = tmp.dadd(tmp11)
        tmp11 = x1.mulInt(243)
        tmp = tmp.dadd(tmp11)
        #tmp2 = x4.mulInt(9) + x2.mulInt(81) + x0.mulInt(729)
        tmp2 = x4.mulInt(9)
        tmp11 = x2.mulInt(81)
        tmp2 = tmp2.dadd(tmp11)
        tmp11 = x0.mulInt(729)
        tmp2 = tmp2.dadd(tmp11)
        #tmp3 = y5 + y3.mulInt(9) + y1.mulInt(81)
        tmp3 = x5 + x3.mulInt(9)
        tmp11 = x1.mulInt(81)
        tmp3 = tmp3.dadd(tmp11)
        #tmp4 = y4.mulInt(3) + y2.mulInt(27) + y0.mulInt(243)
        tmp4 = x4.mulInt(3)
        tmp11 = x2.mulInt(27)
        tmp4 = tmp4.dadd(tmp11)
        tmp11 = x0.mulInt(243)
        tmp4 = tmp4.dadd(tmp11)
        b = (tmp + tmp2).toom6hMul(tmp3 + tmp4)
        c = (tmp2.dsub(tmp)).toom6hMul(tmp4.dsub(tmp3))
        #tmp = x5.mulInt(243) + x3.mulInt(27) + x1.mulInt(3)
        tmp = x5.mulInt(243)
        tmp11 = x3.mulInt(27)
        tmp = tmp.dadd(tmp11)
        tmp11 = x1.mulInt(3)
        tmp = tmp.dadd(tmp11)
        #tmp2 = x4.mulInt(81) + x2.mulInt(9) + x0
        tmp2 = x4.mulInt(81)
        tmp11 = x2.mulInt(9) + x0
        tmp2 = tmp2.dadd(tmp11)
        d = (tmp + tmp2).toom6hSqr()
        e = (tmp2.dsub(tmp)).toom6hSqr()
        tmp = x5.mulInt(2)
        tmp11 = x3.mulInt(8)
        tmp = tmp.dadd(tmp11)
        tmp11 = x1.mulInt(32)
        tmp = tmp.dadd(tmp11)
        #tmp2 = x4.mulInt(4) + x2.mulInt(16) + x0.mulInt(64)
        tmp2 = x4.mulInt(4)
        tmp11 = x2.mulInt(16)
        tmp2 = tmp2.dadd(tmp11)
        tmp11 = x0.mulInt(64)
        tmp2 = tmp2.dadd(tmp11)
        #tmp3 = y5 + y3.mulInt(4) + y1.mulInt(16)
        tmp3 = x5 + x3.mulInt(4)
        tmp11 = x1.mulInt(16)
        tmp3 = tmp3.dadd(tmp11)
        #tmp4 = y4.mulInt(2) + y2.mulInt(8) + y0.mulInt(32)
        tmp4 = x4.mulInt(2)
        tmp11 = x2.mulInt(8)
        tmp4 = tmp4.dadd(tmp11)
        tmp11 = x0.mulInt(32)
        tmp4 = tmp4.dadd(tmp11)
        f = (tmp2 + tmp).toom6hMul(tmp4 + tmp3)
        g = (tmp2.dsub(tmp)).toom6hMul(tmp4.dsub(tmp3))
        #tmp = x5.mulInt(32) + x3.mulInt(8) + x1.mulInt(2)
        tmp = x5.mulInt(32)
        tmp11 = x3.mulInt(8)
        tmp = tmp.dadd(tmp11)
        tmp11 = x1.mulInt(2)
        tmp = tmp.dadd(tmp11)
        #tmp2 = x4.mulInt(16) + x2.mulInt(4) + x0
        tmp2 = x4.mulInt(16) + x0
        tmp11 = x2.mulInt(4)
        tmp2 = tmp2.dadd(tmp11)
        h = (tmp2 + tmp).toom6hSqr()
        i = (tmp2.dsub(tmp)).toom6hSqr()
        tmp = x5.dadd(x3)
        tmp = tmp.dadd(x1)
        tmp2 = x4.dadd(x2)
        tmp2 = tmp2.dadd(x0)
        j = (tmp2 + tmp).toom6hSqr()
        k = (tmp2.dsub(tmp)).toom6hSqr()
        tmp = j + k
        tmp = tmp.dmulInt(50)
        tmp2 = j.dsub(k)
        tmp2 = tmp2.dmulInt(50)
        tmp3 = h + i
        tmp3 = tmp3.dmulInt(8)
        tmp4 = h.dsub(i)
        tmp4 = tmp4.dmulInt(16)
        tmp5 = f + g
        tmp5 = tmp5.dmulInt(16)
        tmp6 = f.dsub(g)
        tmp6 = tmp6.dmulInt(8)
        tmp7 = d + e
        tmp8 = d.dsub(e)
        tmp9 = b + c
        tmp10 = b.dsub(c)
        #result = tmp.mulInt(105) - tmp3.mulInt(12) - tmp5.mulInt(3) + tmp7.mulInt(3) + tmp9
        result = tmp.mulInt(105)
        tmp11 = tmp3.mulInt(12)
        result = result.dsub(tmp11)
        tmp11 = tmp5.mulInt(3)
        result = result.dsub(tmp11)
        tmp11 = tmp7.mulInt(3) + tmp9
        result = result.dadd(tmp11)
        result = result.ddivInt(480)
        result = result.ddivInt(350) - z0
        #z9 = tmp2.mulInt(315) - tmp4.mulInt(36) - tmp6.mulInt(9) + tmp8.mulInt(27) + tmp10
        z9 = tmp2.mulInt(315)
        tmp11 = tmp4.mulInt(36)
        z9 = z9.dsub(tmp11)
        tmp11 = tmp6.mulInt(9)
        z9 = z9.dsub(tmp11)
        tmp11 = tmp8.mulInt(27) + tmp10
        z9 = z9.dadd(tmp11)
        z9 = z9.ddivInt(875)
        z9 = z9.ddivInt(576)
        #z5 = tmp2.mulInt(733) - tmp4.mulInt(26) - tmp6.mulInt(26) + tmp8.mulInt(9) + tmp10.mulInt(3)
        z5 = tmp2.mulInt(733)
        tmp11 = tmp4 + tmp6
        tmp11 = tmp11.dmulInt(26)
        z5 = z5.dsub(tmp11)
        tmp11 = tmp8.mulInt(9)
        z5 = z5.dadd(tmp11)
        tmp11 = tmp10.mulInt(3)
        z5 = z5.dadd(tmp11)
        z5 = z5.ddivInt(288)
        z5 = z5.ddivInt(100)
        #z1 = tmp2.mulInt(105) - tmp4.mulInt(3) - tmp6.mulInt(12) + tmp8 + tmp10.mulInt(3)
        z1 = tmp2.mulInt(105)
        tmp11 = tmp4.mulInt(3)
        z1 = z1.dsub(tmp11)
        tmp11 = tmp6.mulInt(12)
        z1 = z1.dsub(tmp11)
        tmp11 = tmp8 + tmp10.mulInt(3)
        z1 = z1.dadd(tmp11)
        z1 = z1.ddivInt(480)
        z1 = z1.ddivInt(350)
        tmp2.dneg()
        tmp2 = tmp2.dmulInt(35)
        tmp2 = tmp2.dmulInt(481)
        tmp4 = tmp4.dmulInt(2)
        tmp6 = tmp6.dmulInt(2)
        #z7 = tmp2 + tmp4.mulInt(746) + tmp6.mulInt(254) - tmp8.mulInt(579) - tmp10.mulInt(57)
        z7 = tmp2 + tmp4.mulInt(746)
        tmp11 = tmp6.mulInt(254)
        z7 = z7.dadd(tmp11)
        tmp11 = tmp8.mulInt(579)
        z7 = z7.dsub(tmp11)
        tmp11 = tmp10.mulInt(57)
        z7 = z7.dsub(tmp11)
        z7 = z7.ddivInt(768)
        z7 = z7.ddivInt(525)
        z7 = z7.ddivInt(5)
        #z3 = tmp2 + tmp4.mulInt(254) + tmp6.mulInt(746) - tmp8.mulInt(171) - tmp10.mulInt(193)
        tmp11 = tmp4.dmulInt(254)
        z3 = tmp2.dadd(tmp11)
        tmp11 = tmp6.dmulInt(746)
        z3 = z3.dadd(tmp11)
        tmp11 = tmp8.dmulInt(171)
        z3 = z3.dsub(tmp11)
        tmp11 = tmp10.dmulInt(193)
        z3 = z3.dsub(tmp11)
        z3 = z3.ddivInt(768)
        z3 = z3.ddivInt(525)
        z3 = z3.ddivInt(5)
        tmp5 = tmp5.dmulInt(2)
        #z2 = -z0.mulInt(400).mulInt(35).mulInt(517) + tmp.mulInt(315) - tmp3.mulInt(9) - tmp5.mulInt(18) + tmp7 + tmp9.mulInt(27)
        z2 = -z0.mulInt(400)
        z2 = z2.dmulInt(35)
        z2 = z2.dmulInt(517)
        tmp11 = tmp.mulInt(315)
        z2 = z2.dadd(tmp11)
        tmp11 = tmp3.mulInt(9)
        z2 = z2.dsub(tmp11)
        tmp11 = tmp5.mulInt(18)
        z2 = z2.dsub(tmp11)
        tmp11 = tmp7 + tmp9.mulInt(27)
        z2 = z2.dadd(tmp11)
        z2 = z2.ddivInt(875)
        z2 = z2.ddivInt(576)
        tmp3 = tmp3.dmulInt(2)
        #z6 = -z0.mulInt(160).mulInt(649).mulInt(15) + tmp.mulInt(733) - tmp3.mulInt(13) - tmp5.mulInt(13) + tmp7.mulInt(3) + tmp9.mulInt(9)
        z6 = -z0.mulInt(160)
        z6 = z6.dmulInt(649)
        z6 = z6.dmulInt(15)
        tmp11 = tmp.mulInt(733)
        z6 = z6.dadd(tmp11)
        tmp11 = tmp3 + tmp5
        tmp11 = tmp11.dmulInt(13)
        z6 = z6.dsub(tmp11)
        tmp11 = tmp7.mulInt(3)
        z6 = z6.dadd(tmp11)
        tmp11 = tmp9.mulInt(9)
        z6 = z6.dadd(tmp11)
        z6 = z6.ddivInt(288)
        z6 = z6.ddivInt(100)
        tmp = tmp.dmulInt(35)
        tmp = tmp.dmulInt(481)
        #z8 = z0.mulInt(320).mulInt(175).mulInt(517) - tmp + tmp3.mulInt(746) + tmp5.mulInt(254) - tmp7.mulInt(193) - tmp9.mulInt(171)
        z8 = z0.mulInt(320)
        z8 = z8.dmulInt(175)
        z8 = z8.dmulInt(517)
        tmp11 = tmp3.mulInt(746) - tmp
        z8 = z8.dadd(tmp11)
        tmp11 = tmp5.mulInt(254)
        z8 = z8.dadd(tmp11)
        tmp11 = tmp7.mulInt(193)
        z8 = z8.dsub(tmp11)
        tmp11 = tmp9.mulInt(171)
        z8 = z8.dsub(tmp11)
        z8 = z8.ddivInt(768)
        z8 = z8.ddivInt(525)
        z8 = z8.ddivInt(5)
        #z4 = z0.mulInt(480).mulInt(350).mulInt(649) - tmp + tmp3.mulInt(254) + tmp5.mulInt(746) - tmp7.mulInt(57) - tmp9.mulInt(579)
        z4 = z0.mulInt(480)
        z4 = z4.dmulInt(350)
        z4 = z4.dmulInt(649)
        z4 = z4.dsub(tmp)
        tmp11 = tmp3.dmulInt(254)
        z4 = z4.dadd(tmp11)
        tmp11 = tmp5.dmulInt(746) 
        z4 = z4.dadd(tmp11)
        tmp11 = tmp7.dmulInt(57)
        z4 = z4.dsub(tmp11)
        tmp11 = tmp9.dmulInt(579)
        z4 = z4.dsub(tmp11)
        z4 = z4.ddivInt(768)
        z4 = z4.ddivInt(525)
        z4 = z4.ddivInt(5)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z9)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z8)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z7)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z6)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z5)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z4)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z3)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z2)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z1)
        result.limbs.insert(zeros, 0)
        result = result.udadd(z0)
        result.sign = true

# Multiplication. Used algorithms are changed with limbs length.
# Karatsuba and Toom Cook multiplication are slow when the sizes of two operands are different, 
# so filling zeros to smaller value.        
proc `*` *(x, y: BigInt): BigInt =
    ## Returns the product of two BigInts.
    var 
        m: int = len(x.limbs)
        n: int = len(y.limbs)
    if n > m:
        result = y * x
    elif x == y:
        if n < KARATSUBA_THRESHOLD:
            result = x.schoolbookSqr()
        elif n < TOOM3_THRESHOLD:
            result = x.karatsubaSqr()
        elif n < TOOM4_THRESHOLD:
            result = x.toom3Sqr()
        elif n < TOOM6H_THRESHOLD * 50:
            result = x.toom4Sqr()
        else:
            result = x.toom6hSqr()
    elif n == m:
        if n < KARATSUBA_THRESHOLD:
            result = x.schoolbookMul(y)
            #[
            if n < KARATSUBA_THRESHOLD*5 div 16:
                result = x.schoolbookMul(y)
            elif n < KARATSUBA_THRESHOLD*5 div 8:
                result = x.schoolbookMulStatic4(y)
            elif n < KARATSUBA_THRESHOLD*3 div 4:
                result = x.schoolbookMulStatic2(y)
            elif n < KARATSUBA_THRESHOLD*7 div 8:
                result = x.schoolbookMulStatic3(y)
            else:
                result = x.schoolbookMulStatic(y)
            ]#
        elif n < TOOM3_THRESHOLD:
            result = x.karatsubaMul(y)
        elif n < TOOM4_THRESHOLD:
            result = x.toom3Mul(y)
        elif n < TOOM6H_THRESHOLD:
            result = x.toom4hMul(y)
        else:
            result = x.toom6hMul(y)
    else:
        if n < KARATSUBA_THRESHOLD:
            result = x.schoolbookMul(y)
        elif n < TOOM3_THRESHOLD:
            var y2: BigInt = BigInt(sign: y.sign, limbs: concat(repeat(0'i64, (m - n)), y.limbs))
            result = x.karatsubaMul(y2)
            result.limbs.delete(0,(m - n - 1))
        elif n < TOOM4_THRESHOLD:
            var y2: BigInt = BigInt(sign: y.sign, limbs: concat(repeat(0'i64, (m - n)), y.limbs))
            result = x.toom3Mul(y2)
            result.limbs.delete(0,(m - n - 1))
        elif n < TOOM6H_THRESHOLD:
            var y2: BigInt = BigInt(sign: y.sign, limbs: concat(repeat(0'i64, (m - n)), y.limbs))
            result = x.toom4hMul(y2)
            result.limbs.delete(0,(m - n - 1))
        else:
            var y2: BigInt = BigInt(sign: y.sign, limbs: concat(repeat(0'i64, (m - n)), y.limbs))
            result = x.toom6hMul(y2)
            result.limbs.delete(0,(m - n - 1))
            
proc `*` *(x: BigInt, y: SomeInteger): BigInt =
    ## Returns the product of a BigInt and an integer.
    result = x * newBigInt(y)

proc `*` *(x: SomeInteger, y: BigInt): BigInt =
    ## Returns the product of an integer and a BigInt.
    result = newBigInt(x) * y

proc `+=` *[T: SomeInteger|BigInt](x: var BigInt, y: T) =
    ## x is overwritten by x + y.
    x = x + y

proc `-=` *[T: SomeInteger|BigInt](x: var BigInt, y: T) =
    ## x is overwritten by x - y.
    x = x - y

proc `*=` *[T: SomeInteger|BigInt](x: var BigInt, y: T) =
    ## x is overwritten by x * y.
    x = x * y

proc `^` *(x: BigInt, y: SomeInteger): BigInt =
    ## Returns x to the yth power.
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
    ## Returns x to the yth power.
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
    ## Set precision of BigFloat.
    bfContext.prec = prec

proc getPrec*(): int =
    ## Get current precision of BigFloat.
    result = bfContext.prec

proc truncate(x: BigFloat): BigFloat =
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
    ## Constructs a new BigFloat object from a string.
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
    ## Constructs a new BigFloat object from a BigInt.
    result = new BigFloat
    result.intPart = a
    result.exp = LOG_BASE * (len(result.intPart.limbs) - 1) + len($result.intPart.limbs[^1]) - 1
    result = result.truncate()

proc newBigFloat*(a: SomeInteger): BigFloat =
    ## Constructs a new BigFloat object from an integer.
    result = newBigFloat(newBigInt(a))

proc toStr(x: BigFloat): string = 
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
    ## Converts a BigFloat to a string.
    result = x.toStr()

proc `-` *(x: BigFloat): BigFloat =
    ## Negates a BigFloat.
    result = new BigFloat
    if x.intPart == zero:
        result = x
    else:
        result.intPart = newBigInt(0)
        result.intPart.limbs = x.intPart.limbs[0..^1]
        result.exp = x.exp
        result.intPart.sign = not x.intPart.sign

proc abs*(x: BigFloat): BigFloat = 
    ## Absolute value of a BigFloat.
    result = new BigFloat
    result.intPart = newBigInt(0)
    result.intPart.limbs = x.intPart.limbs[0..^1]
    result.intPart.sign = true
    result.exp = x.exp
    
proc `+` *(x, y: BigFloat): BigFloat =
    ## Returns the sum of two BigFloats.
    if x.intPart == zero:
        result = y
    elif y.intPart == zero:
        result = x
    elif y.exp > x.exp:
        result = y + x
    else:
        var
            a: int
            b: int
            x2: BigInt
            y2: BigInt
            tmp: string
            zeros: seq[int64]
        if x.exp - y.exp > bfContext.prec:
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
    ## Returns the sum of a BigFloat and a BigInt or an integer.
    result = x + newBigFloat(y)

proc `+` *[T: SomeInteger|BigInt](x: T, y: BigFloat): BigFloat =
    ## Returns the sum of a BigInt or an integer and a BigFloat.
    result = newBigFloat(x) + y

proc `-` *(x, y: BigFloat): BigFloat =
    ## Returns the difference of two BigFloat.
    result = x + (-y)

proc `-` *[T: SomeInteger|BigInt](x: BigFloat, y: T): BigFloat =
    ## Returns the difference of a BigFloat and a BigInt or an integer.
    result = x - newBigFloat(y)

proc `-` *[T: SomeInteger|BigInt](x: T, y: BigFloat): BigFloat =
    ## Returns the difference of a BigInt or an integer and a BigFloat.
    result = newBigFloat(x) - y

proc `*` *(x, y: BigFloat): BigFloat =
    ## Returns the product of two BigFloats.
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
    ## Returns the product of a BigFloat and a BigInt or an integer.
    result = x * newBigFloat(y)

proc `*` *[T: SomeInteger|BigInt](x: T, y: BigFloat): BigFloat =
    ## Returns the product of a BigInt or an integer and a BigFloat.
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
        t: int = (bfContext.prec * 53) div 100
        precList: seq[int] = @[t]
        precOrig: int = bfContext.prec
    while t >= 16:
        t = t div 2
        precList.add(t)
    precList.reverse
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
    for i in 0..(len(precList) - 1):
        t = precList[i] + 16
        setPrec(t)
        result = result.truncate()
        result = result + result * (one - x.truncate() * result)
    setPrec(precOrig + 16)
    result = result.truncate()
    result = result + result * (one - x * result)
    setPrec(precOrig)
    result = result.truncate()

proc `/` *(x, y: BigFloat): BigFloat =
    ## x divided by y.
    if y.intPart == zero:
        raise newException(ValueError, "Division by zero.")
    result = x * (y.inv())

proc `/` *[T:SomeInteger|BigInt](x: BigFloat, y: T): BigFloat =
    ## x divided by y.
    result = x / newBigFloat(y)

proc `/` *[T:SomeInteger|BigInt](x: T, y: BigFloat): BigFloat =
    ## x divided by y.
    result = newBigFloat(x) / y

proc `+=` *[T: BigFloat|BigInt|SomeInteger](x: var BigFloat, y: T) = 
    ## x is overwritten by x + y
    x = x + y

proc `-=` *[T: BigFloat|BigInt|SomeInteger](x: var BigFloat, y: T) = 
    ## x is overwritten by x - y
    x = x - y

proc `*=` *[T: BigFloat|BigInt|SomeInteger](x: var BigFloat, y: T) = 
    ## x is overwritten by x * y
    x = x * y

proc `/=` *[T: BigFloat|BigInt|SomeInteger](x: var BigFloat, y: T) = 
    ## x is overwritten by x / y
    x = x / y

# Square root of x by Newton-raphson method.
proc sqrt*(x: BigFloat): BigFloat = 
    ## Square root of a BigFloat.
    var 
        y: BigFloat
        one: BigFloat
        half: BigFloat
        t: int = (bfContext.prec * 53) div 100
        precList: seq[int] = @[t]
        precOrig: int = bfContext.prec
    if not x.intPart.sign:
        raise newException(ValueError, "Negative value for sqrt is not supported.")
    while t >= 16:
        t = t div 2
        precList.add(t)
    precList.reverse
    setPrec(16)
    y = x.truncate()
    one = newBigFloat("1")
    half = newBigFloat("0.5")
    result = newBigFloat("1")
    result.exp += y.exp div 2
    for i in 0..12:
        result = (result * half) + (y * half * result.inv())
    result = result.inv()
    for i in 0..(len(precList) - 1):
        t = precList[i] + 16
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
    ## Returns square root of a BigInt as a BigFloat.
    result = newBigFloat(x).sqrt()

proc `^` *(x: BigFloat, y: SomeInteger): BigFloat =
    ## Returns x to the yth power. Real number exponent is not supported. 
    if y < 0:
        if x.intPart == zero:
            raise newException(ValueError, "Division by zero.")
        else:
            result = (x^abs(y)).inv()
    elif y == 0:
        result = BigFloat(intPart: newBigInt(1), exp: 0)
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
    ## Returns x to the yth power. Real number exponent is not supported. 
    if abs(y) > newBigInt("9223372036854775807"):
        raise newException(ValueError, "Exponent too large.")
    result = x^(($y).parseBiggestInt)

# BigInt division depends on BigFloat division, so implemented here.
# Not tested well. Might be incorrect for some corner cases.
proc `div` *(x, y: BigInt): BigInt =
    ## Returns the quotient of x by y.
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
    ## Returns the quotient of x by y.
    result = x div newBigInt(y)

proc `div` *(x: SomeInteger, y: BigInt): BigInt =
    ## Returns the quotient of x by y.
    result = newBigInt(x) div y

proc `mod` *(x, y: BigInt): BigInt =
    ## Returns the remainder of x by y.
    result = x - y * (x div y)

proc `mod` *(x: BigInt, y: SomeInteger): BigInt =
    ## Returns the remainder of x by y.
    result = x mod newBigInt(y)

proc `mod` *(x: SomeInteger, y: BigInt): BigInt =
    ## Returns the remainder of x by y.
    result = newBigInt(x) mod y

proc divmod*(x, y: BigInt): seq[BigInt] = 
    ## Returns the seq @[x div y, x mod y]
    var t: BigInt 
    t = x div y
    result = @[t, x - y * t]

proc divmod*(x: BigInt, y: SomeInteger): seq[BigInt] =
    ## Returns the seq @[x div y, x mod y]
    result = x.divmod(newBigInt(y))

proc divmod*(x: SomeInteger, y: BigInt): seq[BigInt] =
    ## Returns the seq @[x div y, x mod y]
    result = newBigInt(x).divmod(y)

