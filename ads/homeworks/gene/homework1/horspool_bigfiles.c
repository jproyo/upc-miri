#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#define ASIZE 256

void preBmBc(char *x, int m, long int bmBc[]) {
   int i;
 
   for (i = 0; i < ASIZE; ++i)
      bmBc[i] = m;
   for (i = 0; i < m - 1; ++i)
      bmBc[x[i]] = m - i - 1;
}
long int HORSPOOL(char *x, int m, char *y, int n) {
  long int j, bmBc[ASIZE],num=0;
   char c;
   /* Preprocessing */
    preBmBc(x, m, bmBc);

   /* Searching */
   j = 0;
   while (j <= n - m) {
      c = y[j + m - 1];
      if (x[m - 1] == c && memcmp(x, y + j, m - 1) == 0)
	//printf("j=%d \n",j);
	num++;
      j += bmBc[c];
   }
   return num;
}

int main(int argc, char *argv[])
{

  FILE *f;
  char pattern[256];
  char filename[256];
  char *text,*intertext;
  long int  i,j,l,n,n1,lbuffer,lpattern;
  clock_t initemps;
  if (argc==1){
    printf("    1:: file\n");
    printf("    2:: pattern\n");
    printf("    3::length of the buffer\n");
    exit(-1);
  }
  lbuffer=atoi(argv[3]);
  lpattern=strlen(argv[2]);
  sprintf(pattern,"%s",argv[2]); 
  sprintf(filename,"%s",argv[1]);
  printf("pattern: %s \n",pattern);
  text = (char *) malloc ((lbuffer+1) *sizeof(char));
  intertext = (char *) malloc ((2*lpattern) *sizeof(char));
  if (!(f = fopen(filename, "r"))) {
    fprintf (stderr, "I can't open the file: %s\n", filename);
    exit(1);
  }
  initemps=clock();
  n=0;n1=0;
  l=fread(text,1,lbuffer,f);
  while (!feof(f)){
     j=0;for (i=lbuffer-lpattern+1;i<l;i++){intertext[j]=text[i];j++;}
    n+=HORSPOOL(pattern,lpattern,text,l);
     l=fread(text,1,lbuffer,f);
    for (i=0;i<lpattern-1;i++){intertext[j]=text[i];j++;}intertext[j]='\0';
     n1+=HORSPOOL(pattern,lpattern,intertext,2*lpattern-2);
   }
  n+=HORSPOOL(pattern,strlen(pattern),text,l);
  printf("total=%ld\n",n+n1);
  printf( "Time: %f s\n", (clock()-initemps)/(double)CLOCKS_PER_SEC );
  fclose(f);
  free(text);
  return 0;
}
