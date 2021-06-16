#include <iostream>
#include <math.h>
#include <gmpxx.h>
#include <iomanip>
#include <chrono>

mpz_class factorial1(int), factorialCore(int, int), factorial2(int);
void sqrt_bench(long n);

mpz_class factorial1(int n)
{
    mpz_class result = 1;
    for(int i = 1; i <= n; i++)
    {
        result *= i;
    }
    return result;
}

mpz_class factorialCore(int a, int b) 
{
    mpz_class result = 1;
    if(b == a + 1)
    {
        return result;
    }
    else
    {
        int m = (a + b) / 2;
        mpz_class am = factorialCore(a, m);
        mpz_class mb = factorialCore(m, b);
        result = am * mb;
        return result;
    }
}

mpz_class factorial2(int n)
{
    mpz_class result = 1;
    if((n == 0) or (n == 1))
    {
        return result;
    }
    else
    {
        result = factorialCore(0, n);
        return result;
    }
}

void sqrt_bench(long n)
{
    mpf_set_default_prec(n);
    mpf_class a = 2;
    mpf_class x = sqrt(a);
}

int main()
{
    mpz_class n = 1;
    std::chrono::system_clock::time_point t1, t2;
    double t;
    long k;
    // small digits addition.
    t1 = std::chrono::system_clock::now();
    for(int i = 1; i <= 10000000; i++)
    {
        n += i;
    }
    t2 = std::chrono::system_clock::now();
    t = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count() / 1000000.0;
    std::cout << "Sum of 1 to 10000000: ";
    std::cout << t;
    std::cout << " seconds." << std::endl;
    std::cout << std::endl;
    // small digits multiplication.
    n = 123456789;
    mpz_class tmp = n;
    t1 = std::chrono::system_clock::now();
    for(int i = 1; i <= 10000000; i++)
    {
        tmp *= i;
        tmp = n;
    }
    t2 = std::chrono::system_clock::now();
    t = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count() / 1000000.0;
    std::cout << "10000000 replicates of small digits multiplication: ";
    std::cout << t;
    std::cout << " seconds." << std::endl;
    std::cout << std::endl;
    // Five to the mth power.
    int m = 5000;
    n = 5;
    for(int i = 0; i <= 5; i++)
    {
        t1 = std::chrono::system_clock::now();
        mpz_pow_ui(tmp.get_mpz_t(), n.get_mpz_t(), m);
        t2 = std::chrono::system_clock::now();
        t = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count() / 1000000.0;
        std::cout << "Five to the ";
        std::cout << m;
        std::cout << "th power: ";
        std::cout << t;
        std::cout << " seconds." << std::endl;
        
        if(i % 2 == 0)
        {
            m *= 2;
        }
        else
        {
            m *= 5;
        }
    }
    std::cout << std::endl;
    // naive factorial with loop.
    m = 1000;
    for(int i = 0; i <= 3; i++)
    {
        t1 = std::chrono::system_clock::now();
        tmp = factorial1(m);
        t2 = std::chrono::system_clock::now();
        t = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count() / 1000000.0;
        std::cout << "Factorial of ";
        std::cout << m;
        std::cout << " by loop: ";
        std::cout << t;
        std::cout << " seconds." << std::endl;

        if(i % 2 == 1)
        {
            m *= 2;
        }
        else
        {
            m *= 5;
        }
    }
    std::cout << std::endl;
    // factorial with binary splitting.
    m = 1000;
    for(int i = 0; i <= 5; i++)
    {
        t1 = std::chrono::system_clock::now();
        tmp = factorial2(m);
        t2 = std::chrono::system_clock::now();
        t = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count() / 1000000.0;
        std::cout << "Factorial of ";
        std::cout << m;
        std::cout << " by binary splitting: ";
        std::cout << t;
        std::cout << " seconds." << std::endl;
        if(i % 2 == 1)
        {
            m *= 2;
        }
        else
        {
            m *= 5;
        }
    }
    std::cout << std::endl;
    // sqrt(2).
    k = 100000;
    for(int i = 0; i <= 2; i++)
    {
        t1 = std::chrono::system_clock::now();
        sqrt_bench(k * log2(10));
        t2 = std::chrono::system_clock::now();
        t = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1).count() / 1000000.0;
        std::cout << "sqrt(2) ";
        std::cout << k;
        std::cout << " digits: ";
        std::cout << t;
        std::cout << " seconds." << std::endl;
        k *= 10;
    }
    return 0;
}

    

