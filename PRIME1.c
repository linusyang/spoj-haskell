/* Copyright (c) 2015 Linus Yang */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXP 32000

void show_prime(char *ptable, int *primes, int primen, int x, int y)
{
    int i, j, p, start, maxi;

    if (x > y) {
        return;
    }
    if (y <= MAXP) {
        for (i = x; i <= y; i++) {
            if (ptable[i]) {
                printf("%d\n", i);
            }
        }
    } else {
        maxi = y - x + 1;
        ptable = (char *) malloc(maxi);
        memset(ptable, 1, maxi);
        for (j = 0; j < primen; j++) {
            p = primes[j];
            if (p * p > y) {
                break;
            }
            if (p >= x) {
                start = p + p;
            } else {
                start = x + ((p - x % p) % p);
            }
            for (i = start; i <= y; i += p) {
                ptable[i - x] = 0;
            }
        }
        start = x % 2 ? x : x + 1;
        if (x == 2) {
            printf("2\n");
        }
        for (i = start; i <= y; i += 2) {
            if (ptable[i - x]) {
                printf("%d\n", i);
            }
        }
        free(ptable);
    }
}

int main(void)
{
    int n, i, j, x, y;
    char *ptable;
    int *primes, primen = 0;

    primes = (int *) malloc(sizeof(int) * MAXP);
    ptable = (char *) malloc(MAXP);
    memset(ptable, 1, MAXP);
    ptable[0] = 0;
    ptable[1] = 0;

    for (i = 2; i < MAXP; i++) {
        if (ptable[i]) {
            primes[primen++] = i;
            for (j = i + i; j < MAXP; j += i) {
                ptable[j] = 0;
            }    
        }
    }

    scanf("%d", &n);
    for (i = 0; i < n; i++) {
        if (i) {
            printf("\n");
        }
        scanf("%d %d", &x, &y);
        if (x > y) {
            continue;
        }
        if (x < 2) {
            x = 2;
        }
        show_prime(ptable, primes, primen, x, y);
    }
    
    free(ptable);
    free(primes);

    return 0;
}
