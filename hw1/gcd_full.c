#include <stdio.h>


/* Imperative style modification of example 1.20  */

    int gcdI(int i, int j) {
       while ( i != j) { 
           if (i > j) {
              i = i - j;
           } else {
              j = j - i;
           }
       }      
       return i;
    }

/* Modiification of 1.4 page 38 */
  int  gcdM(int i, int j) {
     while ( i != j) {
        if (i > j) {
           i = i % j;
        } else {
            j = j % i;
        }
     }
   return i; 
 }

/* ADD code for the recursive style implementation */
int  gcdR(int i, int j) {
	while (i != j)
    {
        if (i > j)
        {
            return gcdR(i - j, j);
        }
        else
        {
            return gcdR(i, j - i);
        }
    }
    return i;
}

int main(int argc, char **argv) {
  if (argc < 3) {
    printf("%s usage: [I] [J]\n", argv[0]);
    return 1;
  }
  int i = atoi(argv[1]);
  int j = atoi(argv[2]);
  int r = gcdR(i,j);
  printf("%d\n", r);
  return 0;
}




